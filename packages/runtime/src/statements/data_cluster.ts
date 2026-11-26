import {ABAP} from "..";
import {throwError} from "../throw_error";
import {Character, DecFloat34, FieldSymbol, Float, HashedTable, Hex, HexUInt8, Integer, Integer8, Packed, String as StringType,
  Structure, Table, XString} from "../types";

declare const abap: ABAP;

// EXPORT ... TO / IMPORT ... FROM a data cluster, DELETE FROM and FREE MEMORY.
// The clusters live in the context of the runtime instance
// (abap.context.dataClusters), one store per medium (a cluster put into
// SHARED MEMORY is not found in SHARED BUFFER):
// - MEMORY: the ABAP memory, a cluster per ID
// - DATABASE, SHARED MEMORY, SHARED BUFFER: a cluster stored under (dbtab,
//   area, id) and, for a client dependent dbtab, the client; no table is
//   written, every program of the runtime instance sees it
// - DATA BUFFER: the byte string names the cluster in the store
//
// What is stored is a copy of each parameter's value at the time of the
// EXPORT. A later EXPORT with the same key replaces the whole cluster. An
// IMPORT of a key that holds no cluster sets sy-subrc 4 and leaves its targets
// alone; otherwise sy-subrc is 0 and every parameter the cluster holds is
// imported into its target, a parameter it does not hold leaves its target
// alone. The import follows the conversion rules of IMPORT: a c field of
// another length is cut or padded, a structure gets the components in their
// order and initial values in extra last components, a table gets the rows
// (the header line of a table keeps its value), and a target of another kind
// of type raises CX_SY_IMPORT_MISMATCH_ERROR.
//
// Not modelled: the conversion of the ID to the length of the dbtab's key
// field (an ID longer than that field is not cut), the displacement of
// SHARED BUFFER clusters when the buffer is full and the refusal of
// references inside an exported value. The store is the runtime instance's:
// a new instance starts with no clusters, the way an application server's
// restart empties its shared memory.

export type DataClusterMedium = "MEMORY" | "DATABASE" | "SHARED MEMORY" | "SHARED BUFFER" | "DATA BUFFER";

export interface IDataClusterKey {
  medium: DataClusterMedium;
  table?: string;
  area?: string;
  id?: any;
  client?: any;
  /** DATA BUFFER: the byte string */
  buffer?: any;
}

export interface IExportClusterOptions extends IDataClusterKey {
  parameters: {[name: string]: any};
}

export interface IImportClusterOptions extends IDataClusterKey {
  parameters: {[name: string]: any};
}

const BUFFER_MARK = "DATA BUFFER ";

function storeOf(medium: string): Map<string, Map<string, any>> {
  const stores = abap.context.dataClusters;
  let store = stores.get(medium);
  if (store === undefined) {
    store = new Map();
    stores.set(medium, store);
  }
  return store;
}

function plain(value: any): string {
  if (value === undefined) {
    return "";
  }
  if (value instanceof FieldSymbol) {
    return plain(value.getPointer());
  }
  if (typeof value === "string" || typeof value === "number") {
    return String(value);
  }
  return String(value.get());
}

function keyOf(options: IDataClusterKey): string {
  const id = plain(options.id).replace(/ +$/, "");
  if (options.medium === "MEMORY") {
    return id;
  }
  const client = options.client === undefined ? abap.builtin.sy.get().mandt.get() : plain(options.client);
  return [
    options.table?.toUpperCase(),
    client,
    options.area?.toUpperCase(),
    id,
  ].join("\n");
}

// DATA BUFFER: the byte string holds the key of the cluster
function bufferKeyOf(buffer: any): string {
  const hex = plain(buffer);
  if (hex === "") {
    throw new Error("IMPORT_FROM_DATA_BUFFER_EMPTY, IMPORT from an initial byte string");
  }
  let key = "";
  for (let i = 0; i + 1 < hex.length; i += 2) {
    key += String.fromCharCode(parseInt(hex.substring(i, i + 2), 16));
  }
  return key;
}

function copyOf(value: any): any {
  if (value instanceof FieldSymbol) {
    return copyOf(value.getPointer());
  }
  if (typeof value === "string" || typeof value === "number") {
    return value;
  }
  return value.clone();
}

// the kind of an elementary type, an IMPORT into another kind raises CX_SY_IMPORT_MISMATCH_ERROR
function kindOf(value: any): string {
  if (value instanceof Character || value instanceof StringType) {
    return "C";
  } else if (value instanceof Integer || value instanceof Integer8 || value instanceof Packed
      || value instanceof Float || value instanceof DecFloat34) {
    return "N";
  } else if (value instanceof Hex || value instanceof HexUInt8 || value instanceof XString) {
    return "X";
  }
  return value?.constructor?.name;
}

function importValue(stored: any, target: any): void {
  if (target instanceof FieldSymbol) {
    importValue(stored, target.getPointer());
  } else if (target instanceof Table || target instanceof HashedTable) {
    if (!(stored instanceof Table) && !(stored instanceof HashedTable)) {
      throwError("CX_SY_IMPORT_MISMATCH_ERROR");
    }
    // clear() and appendThis() work on the rows, the header line keeps its value
    target.clear();
    for (const row of stored.array()) {
      const copy = target.getRowType().clone();
      importValue(row, copy);
      target.appendThis(copy);
    }
  } else if (target instanceof Structure) {
    if (!(stored instanceof Structure)) {
      throwError("CX_SY_IMPORT_MISMATCH_ERROR");
    }
    const from = Object.values(stored.get());
    const into = Object.values(target.get());
    if (from.length > into.length) {
      throwError("CX_SY_IMPORT_MISMATCH_ERROR");
    }
    for (let i = 0; i < into.length; i++) {
      if (i < from.length) {
        importValue(from[i], into[i]);
      } else {
        (into[i] as any).clear();
      }
    }
  } else {
    if (stored instanceof Table || stored instanceof HashedTable || stored instanceof Structure
        || kindOf(stored) !== kindOf(target)) {
      throwError("CX_SY_IMPORT_MISMATCH_ERROR");
    }
    // set() copies the value: a change to the target does not reach the next IMPORT
    target.set(stored);
  }
}

export function exportCluster(options: IExportClusterOptions): void {
  const cluster = new Map<string, any>();
  for (const name of Object.keys(options.parameters)) {
    cluster.set(name.toUpperCase(), copyOf(options.parameters[name]));
  }
  const store = storeOf(options.medium);
  if (options.medium === "DATA BUFFER") {
    const key = BUFFER_MARK + (store.size + 1);
    store.set(key, cluster);
    let hex = "";
    for (const c of key) {
      hex += c.charCodeAt(0).toString(16).padStart(2, "0").toUpperCase();
    }
    options.buffer.set(hex);
    return;
  }
  store.set(keyOf(options), cluster);
}

export function importCluster(options: IImportClusterOptions): void {
  const key = options.medium === "DATA BUFFER" ? bufferKeyOf(options.buffer) : keyOf(options);
  const cluster = storeOf(options.medium).get(key);
  if (cluster === undefined && options.medium === "DATA BUFFER") {
    throw new Error("IMPORT_FORMAT_ERROR, the byte string holds no data cluster");
  } else if (cluster === undefined) {
    abap.builtin.sy.get().subrc.set(4);
    return;
  }
  for (const name of Object.keys(options.parameters)) {
    const stored = cluster.get(name.toUpperCase());
    if (stored === undefined) {
      continue;
    }
    importValue(stored, options.parameters[name]);
  }
  abap.builtin.sy.get().subrc.set(0);
}

/** DELETE FROM MEMORY ID id, DELETE FROM DATABASE / SHARED MEMORY / SHARED BUFFER: sy-subrc 4 when there was no cluster */
export function deleteCluster(options: IDataClusterKey): void {
  const found = storeOf(options.medium).delete(keyOf(options));
  abap.builtin.sy.get().subrc.set(found ? 0 : 4);
}

/** FREE MEMORY [ID id]: the cluster of the ID, or the whole ABAP memory */
export function freeMemory(options: {id?: any}): void {
  if (options.id === undefined) {
    storeOf("MEMORY").clear();
  } else {
    storeOf("MEMORY").delete(keyOf({medium: "MEMORY", id: options.id}));
  }
}
