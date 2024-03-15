import {INumeric} from "./_numeric";
import {ICharacter} from "./_character";
import {Integer} from "./integer";
import {ABAPObject} from "./abap_object";
import {String} from "./string";
import {clone} from "../clone";
import {Structure} from "./structure";
import {FieldSymbol} from "./field_symbol";
import {DataReference} from "./data_reference";
import {insertInternal} from "../statements/insert_internal";
import {sort} from "../statements/sort";
import {Character} from "./character";
import {Hex} from "./hex";
import {HexUInt8} from "./hex_uint8";

// const FEATURE_SHARED_TABLES = true;

export enum TableAccessType {
  standard = "STANDARD",
  sorted = "SORTED",
  hashed = "HASHED",
  index = "INDEX",
  any = "ANY",
}

export enum TableKeyType {
  default = "DEFAULT",
  user = "USER",
  empty = "EMPTY",
}

export class LoopController {
  public index: number;
  public loopTo: number;
  public array: any[];

  public constructor(from: number, loopTo: number, array: any[]) {
    this.index = from;
    this.loopTo = loopTo;
    this.array = array;
  }
}

export type ITableKey = {
  name: string,
  type?: TableAccessType,
  keyFields: string[],
  isUnique: boolean,
};

export type ITableOptions = {
  withHeader: boolean,
  keyType: TableKeyType,
  primaryKey?: ITableKey,
  secondary?: ITableKey[],
};

export type TableRowType = INumeric | Structure | ICharacter | Table | ABAPObject;

interface ITable {
  getQualifiedName(): string | undefined;
  getOptions(): ITableOptions | undefined;
  getRowType(): TableRowType;
  getArrayLength(): number;
  clear(): void;
  set(tab: Table | HashedTable | TableRowType): ITable;
  getHeader(): TableRowType;
}

export class TableFactory {
  public static construct(rowType: TableRowType, options?: ITableOptions, qualifiedName?: string) {
    if (options === undefined) {
      options = {
        primaryKey: {
          name: "primary_key",
          type: TableAccessType.standard,
          keyFields: [],
          isUnique: false,
        },
        keyType: TableKeyType.default,
        withHeader: false,
      };
    }

    if (options.primaryKey?.type === TableAccessType.hashed) {
      return new HashedTable(rowType, options, qualifiedName);
    } else {
      return new Table(rowType, options, qualifiedName);
    }
  }
}

/*
export class SortedTable {
  // todo
}
*/

export class HashedTable implements ITable {
  // @ts-ignore
  private value: {[hash: string]: TableRowType};
  private readonly header: TableRowType | undefined;
  private readonly rowType: TableRowType;
  private readonly loops: Set<LoopController>;
  private readonly options: ITableOptions;
  private readonly qualifiedName: string | undefined;
  private secondaryIndexes: {[name: string]: TableRowType[]};

  public constructor(rowType: TableRowType, options: ITableOptions, qualifiedName?: string) {
    this.value = {};
    this.secondaryIndexes = {};
    this.loops = new Set();
    this.rowType = rowType;
    this.options = options;
    this.options = options;

    if (options?.withHeader === true) {
      this.header = clone(this.rowType);
    }

    this.qualifiedName = qualifiedName?.toUpperCase();
  }

  public getArrayLength() {
    return Object.keys(this.value).length;
  }

  public getKeyByName(name: string) {
    return this.getOptions()?.secondary?.find(s => s.name.toUpperCase() === name.toUpperCase());
  }

  public getSecondaryIndex(name: string) {
    if (this.secondaryIndexes[name.toUpperCase()]) {
      return this.secondaryIndexes[name.toUpperCase()];
    }

    const secondary = this.getKeyByName(name);
    if (secondary === undefined) {
      throw `Table, secondary key "${name}" not found`;
    }
    // note, array() already is a copy, so it can be used,
    const copy = this.array();
    sort(copy as any, {by: secondary.keyFields.map(k => {return {component: k.toLowerCase()};})});

    this.secondaryIndexes[name.toUpperCase()] = copy;
    return copy;
  }

  public buildHashFromData(data: TableRowType): string {
    let hash = "";
    for (const k of this.options.primaryKey!.keyFields) {
      if (k === "TABLE_LINE") {
        if (data instanceof Structure) {
          hash += k + ":" + data.getCharacter() + "|";
        } else {
          // @ts-ignore
          hash += k + ":" + data.get() + "|";
        }
      } else {
        // @ts-ignore
        let val = data.get()[k.toLowerCase()];
        if (val instanceof Structure) {
          val = val.getCharacter();
        } else {
          val = val.get();
        }
        hash += k + ":" + val + "|";
      }
    }
    return hash;
  }

  public deleteIndex(_index: number) {
    throw new Error("HashedTable, deleteIndex");
  }

  public deleteFrom(row: TableRowType) {
    const hash = this.buildHashFromData(row);
    delete this.value[hash];
  }

  public buildHashFromSimple(data: {[key: string]: any}): string {
    let hash = "";
    const ttyp = this.getRowType();
    for (const k of this.options.primaryKey!.keyFields) {
      let val = data[k.toLowerCase()];
      if (val instanceof Structure) {
        val = val.getCharacter();
      } else {
        // convert to correct type, eg Chars have specific length, or rounding,
        if (k === "TABLE_LINE") {
          const rowType = clone(ttyp) as any;
          rowType.set(val.get());
          val = rowType.get();
        } else if (ttyp instanceof Structure) {
          const field = ttyp.get()[k.toLowerCase()];
          // if types match, there is no need to clone
          if (field instanceof String && val instanceof String) {
            val = val.get();
          } else if (field instanceof Character && val instanceof Character && field.getLength() === val.getLength()) {
            val = val.get();
          } else if (field instanceof Hex && val instanceof Hex && field.getLength() === val.getLength()) {
            val = val.get();
          } else if (field instanceof HexUInt8 && val instanceof HexUInt8 && field.getLength() === val.getLength()) {
            val = val.get();
          } else {
            // convert
            const rowType = clone(field) as any;
            rowType.set(val.get());
            val = rowType.get();
          }
        } else {
          throw new Error("HashedTable, buildHashFromSimple, unexpected type");
        }
      }
      hash += k + ":" + val + "|";
    }
    return hash;
  }

  public read(hash: string): TableRowType | undefined {
    return this.value[hash];
  }

  public insert(data: TableRowType): {value: TableRowType | undefined, subrc: number} {
    const hash = this.buildHashFromData(data);

    if (this.value[hash] !== undefined) {
      return {value: undefined, subrc: 4};
    } else {
      const val = this.cloneRow(data);

      for (const loopController of this.loops.values()) {
        loopController.array.push(val);
      }

      this.value[hash] = val;
      return {value: val, subrc: 0};
    }
  }

  public array(): any[] {
    // used for LOOP
    const ret = [];
    for (const hash in this.value) {
      ret.push(this.value[hash]);
    }
    return ret;
  }

  public startLoop(from: number, to: number, array: any[]): LoopController {
    const l = new LoopController(from, to, array);
    this.loops.add(l);
    return l;
  }

  public unregisterLoop(loop: LoopController) {
    this.loops.delete(loop);
  }

  public insertIndex(_item: TableRowType, _index: number) {
    throw new Error("Hash table insert index");
  }

  public append(_item: TableRowType) {
    throw new Error("Hash table append");
  }

  public getQualifiedName(): string | undefined {
    return this.qualifiedName;
  }

  public getOptions(): ITableOptions {
    return this.options;
  }

  public getRowType(): TableRowType {
    return this.rowType;
  }

  public clear(): void {
    this.value = {};
    this.secondaryIndexes = {};
  }

  public set(tab: HashedTable | TableRowType): ITable {
    if (tab instanceof FieldSymbol) {
      if (tab.getPointer() === undefined) {
        throw new Error("GETWA_NOT_ASSIGNED");
      }
      return this.set(tab.getPointer());
    }

    if (tab === this) {
      return this;
    }

    this.clear();
    if (tab instanceof Table || tab instanceof HashedTable) {
      for (const a of tab.array()) {
        this.insert(a);
      }
      return this;
    } else {
      throw new Error("Method not implemented, set hashed table");
    }
  }

  public getHeader(): TableRowType {
    if (this.header === undefined) {
      throw "table, getHeader";
    }
    return this.header;
  }

  ///////////////////////////

  private cloneRow(item: TableRowType) {
    // make sure to do conversion if needed
    if (typeof item === "number") {
      const tmp = clone(this.getRowType());
      tmp.set(new Integer().set(item));
      return tmp;
    } else if (typeof item === "string") {
      const tmp = clone(this.getRowType());
      tmp.set(new String().set(item));
      return tmp;
    // @ts-ignore
    // eslint-disable-next-line max-len
    } else if (this.isStructured === true && item.getQualifiedName && this.rowType.getQualifiedName && item.getQualifiedName() !== "" && item.getQualifiedName() === this.rowType.getQualifiedName()) {
    // types match, so no need to do conversions, just clone the item
      const val = clone(item);
      return val;
    } else {
      const tmp = clone(this.getRowType());
      tmp.set(item);
      return tmp;
    }
  }
}

export class Table implements ITable {
  private value: TableRowType[];
  private readonly header: TableRowType | undefined;
  private readonly rowType: TableRowType;
  private readonly loops: Set<LoopController>;
  private readonly options: ITableOptions;
  private readonly qualifiedName: string | undefined;
  private readonly isStructured: boolean;
  private secondaryIndexes: {[name: string]: TableRowType[]};

  public constructor(rowType: TableRowType, options: ITableOptions, qualifiedName?: string) {
    this.value = [];
    this.secondaryIndexes = {};
    this.loops = new Set();
    this.rowType = rowType;
    this.options = options;
    this.isStructured = rowType instanceof Structure;

    if (options?.withHeader === true) {
      this.header = clone(this.rowType);
    }

    this.qualifiedName = qualifiedName?.toUpperCase();
  }

  public getArrayLength() {
    return this.value.length;
  }

  public getKeyByName(name: string) {
    return this.getOptions()?.secondary?.find(s => s.name.toUpperCase() === name.toUpperCase());
  }

  public getSecondaryIndex(name: string) {
    if (this.secondaryIndexes[name.toUpperCase()]) {
      return this.secondaryIndexes[name.toUpperCase()];
    }

    const secondary = this.getKeyByName(name);
    if (secondary === undefined) {
      throw `Table, secondary key "${name}" not found`;
    }
    const copy = [...this.value];
    sort(copy as any, {by: secondary.keyFields.map(k => {return {component: k.toLowerCase()};}), skipSortedCheck: true});

    this.secondaryIndexes[name.toUpperCase()] = copy;
    return copy;
  }

  public getQualifiedName() {
    return this.qualifiedName;
  }

  public getOptions() {
    return this.options;
  }

  public startLoop(from: number, to: number, array: any[]): LoopController {
    const l = new LoopController(from, to, array);
    this.loops.add(l);
    return l;
  }

  public unregisterLoop(loop: LoopController) {
    this.loops.delete(loop);
  }

  public getRowType() {
    return this.rowType;
  }

  // Modifications to the array must be done inside this class, in order to keep track of LOOP indexes
  public array(): any[] {
    return this.value;
  }

  public clear(): void {
    this.value = [];
    this.secondaryIndexes = {};
  }

  public set(tab: Table | TableRowType) {
    this.secondaryIndexes = {};
    if (this.options?.withHeader === true) {
      this.header?.set(tab);
    } else {
      if (!(tab instanceof Table) && !(tab instanceof FieldSymbol)) {
        throw "Table, set error";
      }

      if (tab instanceof FieldSymbol) {
        tab = tab.getPointer();
      }

      if (tab === this) {
        return this;
      }

      this.clear();

      // this clones the values, and add sorting if required
      insertInternal({table: this, data: tab, lines: true});
    }
    return this;
  }

  public getHeader() {
    if (this.header === undefined) {
      throw "table, getHeader";
    }
    return this.header;
  }

  public insertIndex(item: TableRowType, index: number, noClone = false) {
    this.secondaryIndexes = {};

    if (item instanceof FieldSymbol) {
      const p = item.getPointer();
      if (p === undefined) {
        throw new Error("insertIndex, fs not assigned");
      }
      this.insertIndex(p, index);
      return p;
    }

    let val: TableRowType;
    if (noClone === false) {
      val = this.cloneRow(item);
    } else {
      val = item;
    }

    if (index === 0) {
      this.value.unshift(val);
    } else if (index === this.value.length) {
      this.value.push(val);
    } else {
      this.value.splice(index, 0, val);
    }

    for (const loopController of this.loops.values()) {
      if (index <= loopController.index) {
        loopController.index++;
      }
    }
    return val;
  }

  /** index = javascript indexed */
  public deleteIndex(index: number) {
    this.secondaryIndexes = {};
    if (index > this.value.length) {
      return;
    }
    if (index === this.value.length - 1) {
      this.value.pop(); // pop'ing is faster than splice
    } else if (index === 0) {
      this.value.shift();
    } else {
      this.value.splice(index, 1);
    }
    for (const l of this.loops.values()) {
      if (l.index >= index) {
        l.index--;
      }
    }
  }

  public append(item: TableRowType) {
    this.secondaryIndexes = {};
    if (item instanceof FieldSymbol) {
      const p = item.getPointer();
      if (p === undefined) {
        throw new Error("APPEND, fs not assigned");
      }
      this.append(p);
      return p;
    } else if (item instanceof DataReference) {
      const ref = new DataReference(item.getType());
      ref.assign(item.getPointer());
      this.value.push(ref);
      return ref;
    } else {
      const val = this.cloneRow(item);
      this.value.push(val);
      return val;
    }
  }

  public appendInitial() {
    this.secondaryIndexes = {};
    // note that this will clone the object
    this.append(this.rowType);
    // @ts-ignore
    abap.builtin.sy.get().tabix.set(this.value.length);
    return this.value[this.value.length - 1];
  }

  public sort(compareFn: (a: TableRowType, b: TableRowType) => number) {
    this.value.sort(compareFn);
  }

///////////////////////////

  private cloneRow(item: TableRowType) {
    // make sure to do conversion if needed
    if (typeof item === "number") {
      const tmp = clone(this.getRowType());
      tmp.set(new Integer().set(item));
      return tmp;
    } else if (typeof item === "string") {
      const tmp = clone(this.getRowType());
      tmp.set(new String().set(item));
      return tmp;
    // @ts-ignore
    // eslint-disable-next-line max-len
    } else if (this.isStructured === true && item.getQualifiedName && this.rowType.getQualifiedName && item.getQualifiedName() !== "" && item.getQualifiedName() === this.rowType.getQualifiedName()) {
    // types match, so no need to do conversions, just clone the item
      const val = clone(item);
      return val;
    } else {
      const tmp = clone(this.getRowType());
      tmp.set(item);
      return tmp;
    }
  }

}