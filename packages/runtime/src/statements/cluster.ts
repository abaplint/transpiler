import {Context} from "../context";
import {FieldSymbol, HashedTable, Structure, Table, XString} from "../types";
import {ABAP} from "..";

declare const abap: ABAP;

/** a data cluster: parameter name in upper case -> a copy of the exported data object */
export type DataCluster = Map<string, any>;

const BUFFER_MARK = "GSCLUSTER:";

function pointer(v: any): any {
  return v instanceof FieldSymbol ? v.getPointer() : v;
}

function hexOf(text: string): string {
  return Array.from(text).map(c => c.charCodeAt(0).toString(16).padStart(2, "0")).join("").toUpperCase();
}

function textOf(hex: string): string {
  let ret = "";
  for (let i = 0; i + 1 < hex.length; i += 2) {
    ret += String.fromCharCode(parseInt(hex.substring(i, i + 2), 16));
  }
  return ret;
}

/** the kind of an elementary type, an IMPORT into another kind raises CX_SY_IMPORT_MISMATCH_ERROR */
function kindOf(v: any): string {
  const name = v?.constructor?.name;
  switch (name) {
    case "Character":
    case "String":
      return "C";
    case "Integer":
    case "Integer8":
    case "Packed":
    case "Float":
    case "DecFloat34":
      return "N";
    case "Hex":
    case "XString":
      return "X";
    default:
      return name;
  }
}

async function mismatch() {
  const cls = abap.Classes["CX_SY_IMPORT_MISMATCH_ERROR"];
  if (cls === undefined) {
    throw new Error("CX_SY_IMPORT_MISMATCH_ERROR, IMPORT into a data object of another type");
  }
  throw await new cls().constructor_();
}

/** IMPORT of one parameter: c fields of another length are cut or padded, a structure gets the components in their
 * order and initial values in extra last ones, a table gets the rows (the header line of a table keeps its value) */
async function importValue(source: any, target: any): Promise<void> {
  target = pointer(target);
  if (target instanceof Table || target instanceof HashedTable) {
    if (!(source instanceof Table) && !(source instanceof HashedTable)) {
      return mismatch();
    }
    target.clear();
    for (const row of source.array()) {
      const copy = target.getRowType().clone();
      await importValue(row, copy);
      target.append(copy);
    }
  } else if (target instanceof Structure) {
    if (!(source instanceof Structure)) {
      return mismatch();
    }
    const from = Object.values(source.get());
    const into = Object.values(target.get());
    if (from.length > into.length) {
      return mismatch();
    }
    for (let i = 0; i < into.length; i++) {
      if (i < from.length) {
        await importValue(from[i], into[i]);
      } else {
        (into[i] as any).clear();
      }
    }
  } else {
    if (source instanceof Table || source instanceof HashedTable || source instanceof Structure
        || kindOf(source) !== kindOf(target)) {
      return mismatch();
    }
    target.set(source);
  }
}

/** EXPORT, IMPORT, DELETE FROM MEMORY and FREE MEMORY: the clusters live in the runtime context, ABAP memory under the
 * key "MEMORY|<id>", a database or shared buffer cluster under "DATABASE|<table>|<area>|<id>" or "SHARED|..." */
export class ClusterStatements {
  private readonly context: Context;

  public constructor(context: Context) {
    this.context = context;
  }

  /** EXPORT ... TO <medium>: the cluster under the key is replaced as a whole */
  public exportCluster(key: string, values: {[name: string]: any}): void {
    const cluster: DataCluster = new Map();
    for (const name in values) {
      cluster.set(name.toUpperCase(), pointer(values[name]).clone());
    }
    this.context.clusters.set(key, cluster);
  }

  /** EXPORT ... TO DATA BUFFER xstr: the byte string names the cluster in the runtime's store */
  public exportBuffer(target: XString | FieldSymbol, values: {[name: string]: any}): void {
    const id = ++this.context.clusterBuffers;
    this.exportCluster("BUFFER|" + id, values);
    pointer(target).set(hexOf(BUFFER_MARK + id));
  }

  /** IMPORT ... FROM <medium>: sy-subrc 4 when there is no cluster; a parameter the cluster does not hold keeps
   * the value of its target */
  public async importCluster(key: string, targets: {[name: string]: any}): Promise<void> {
    const cluster = this.context.clusters.get(key);
    if (cluster === undefined) {
      abap.builtin.sy.get().subrc.set(4);
      return;
    }
    for (const name in targets) {
      const value = cluster.get(name.toUpperCase());
      if (value !== undefined) {
        await importValue(value, targets[name]);
      }
    }
    abap.builtin.sy.get().subrc.set(0);
  }

  /** IMPORT ... FROM DATA BUFFER xstr: a byte string that holds no cluster ends the program */
  public async importBuffer(source: XString | FieldSymbol, targets: {[name: string]: any}): Promise<void> {
    const hex = pointer(source).get();
    if (hex === "") {
      throw new Error("IMPORT_FROM_DATA_BUFFER_EMPTY, IMPORT from an initial byte string");
    }
    const text = textOf(hex);
    const key = text.startsWith(BUFFER_MARK) ? "BUFFER|" + text.substring(BUFFER_MARK.length) : undefined;
    if (key === undefined || this.context.clusters.has(key) === false) {
      throw new Error("IMPORT_FORMAT_ERROR, the byte string holds no data cluster");
    }
    await this.importCluster(key, targets);
  }

  /** DELETE FROM MEMORY ID id (and the shared buffer): sy-subrc 4 when there was none */
  public deleteCluster(key: string): void {
    const found = this.context.clusters.delete(key);
    abap.builtin.sy.get().subrc.set(found ? 0 : 4);
  }

  /** FREE MEMORY [ID id]: the cluster of the ID, or all clusters of the ABAP memory */
  public freeMemory(key?: string): void {
    if (key !== undefined) {
      this.context.clusters.delete(key);
      return;
    }
    for (const k of Array.from(this.context.clusters.keys())) {
      if (k.startsWith("MEMORY|")) {
        this.context.clusters.delete(k);
      }
    }
  }

}
