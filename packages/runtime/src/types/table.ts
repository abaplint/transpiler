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

export enum TableAccessType {
  standard = "STANDARD",
  sorted = "SORTED",
  hashed = "HASHED",
  index = "INDEX",
  any = "ANY",
}

export class LoopIndex {
  public index: number;

  public constructor(start: number) {
    this.index = start;
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
  primaryKey?: ITableKey,
  secondary?: ITableKey[],
};

export type TableRowType = INumeric | Structure | ICharacter | Table | ABAPObject;

export class Table  {
  private value: TableRowType[];
  private readonly header: TableRowType | undefined;
  private readonly rowType: TableRowType;
  private readonly loops: Set<LoopIndex>;
  private readonly options: ITableOptions | undefined;
  private readonly qualifiedName: string | undefined;
  private secondaryIndexes: {[name: string]: TableRowType[]};

  public constructor(rowType: TableRowType, options?: ITableOptions, qualifiedName?: string) {
    this.value = [];
    this.secondaryIndexes = {};
    this.loops = new Set();
    this.rowType = rowType;
    this.options = options;

    if (options?.withHeader === true) {
      this.header = clone(this.rowType);
    }

    if (this.options === undefined) {
      this.options = {
        primaryKey: {
          name: "primary_key",
          type: TableAccessType.standard,
          keyFields: [],
          isUnique: false,
        },
        withHeader: false,
      };
    }
    this.qualifiedName = qualifiedName?.toUpperCase();
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
    const copy = clone(this.value);
    sort(copy as any, {by: secondary.keyFields.map(k => {return {component: k.toLowerCase()};})});

    this.secondaryIndexes[name.toUpperCase()] = copy;
    return copy;
  }

  public getQualifiedName() {
    return this.qualifiedName;
  }

  public getOptions() {
    return this.options;
  }

  public startLoop(start: number = 0): LoopIndex {
    const l = new LoopIndex(start);
    this.loops.add(l);
    return l;
  }

  public unregisterLoop(loop: LoopIndex) {
    this.loops.delete(loop);
  }

  public getRowType() {
    return this.rowType;
  }

  // Modifications to the array must be done inside this class, in order to keep track of LOOP indexes
  public array(): readonly any[] {
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
      this.clear();
      if (tab instanceof FieldSymbol) {
        tab = tab.getPointer();
      }
      // this clones the values, and add sorting if required
      insertInternal({table: this, data: tab, lines: true});
    }
  }

  public getHeader() {
    if (this.header === undefined) {
      throw "table, getHeader";
    }
    return this.header;
  }

  public insertIndex(item: TableRowType, index: number) {
    this.secondaryIndexes = {};
    const val = this.getValue(item);
// todo, types might not match?
    if (index === 0) {
      this.value.unshift(val);
    } else {
      this.value.splice(index, 0, val);
    }

    for (const l of this.loops.values()) {
      if (l.index <= index) {
        l.index++;
      }
    }
    return val;
  }

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

  public append(item: TableRowType, cloneRow = true) {
    this.secondaryIndexes = {};
    if (item instanceof FieldSymbol) {
      const p = item.getPointer();
      if (p === undefined) {
        throw new Error("APPEND, fs not assigned");
      }
      this.value.push(p);
      return item;
    } else if (item instanceof DataReference) {
      const ref = new DataReference(item.getType());
      ref.assign(item.getPointer());
      this.value.push(ref);
      return ref;
    } else {
// todoooo
      const val = this.getValue(item, cloneRow);
      const p = clone(this.rowType);
      p.set(val);
      this.value.push(p);
      return p;
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

  private getValue(item: TableRowType, cloneRow = true) {
    // make sure to do conversion if needed
    if (typeof item === "number") {
      const tmp = clone(this.getRowType());
      tmp.set(new Integer().set(item));
      return tmp;
    } else if (typeof item === "string") {
      const tmp = clone(this.getRowType());
      tmp.set(new String().set(item));
      return tmp;
    } else if (cloneRow === true) {
      const tmp = clone(this.getRowType());
      tmp.set(item);
      return tmp;
    } else {
      return item;
    }
  }

}