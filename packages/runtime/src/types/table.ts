import {INumeric} from "./_numeric";
import {ICharacter} from "./_character";
import {Integer} from "./integer";
import {ABAPObject} from "./abap_object";
import {String} from "./string";
import {clone} from "../clone";
import {Structure} from "./structure";
import {FieldSymbol} from "./field_symbol";
import {DataReference} from "./data_reference";

export class LoopIndex {
  public index: number;

  public constructor(start: number) {
    this.index = start;
  }
}

export type ITableOptions = {
  type?: string,
  keyFields?: string[],
  isUnique?: boolean,
  withHeader: boolean,
};

export type TableRowType = INumeric | Structure | ICharacter | Table | ABAPObject;

export class Table  {
  private value: TableRowType[];
  private readonly rowType: TableRowType;
  private readonly loops: Set<LoopIndex>;

  public constructor(rowType: TableRowType, _options?: ITableOptions) {
    this.value = [];
    this.loops = new Set();
    this.rowType = rowType;
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
  }

  public set(tab: Table) {
    this.clear();
    for (const a of tab.array()) {
      // this clones the values
      this.append(a);
    }
  }

  public insertIndex(item: TableRowType, index: number) {
    const val = this.getValue(item);
    this.value.splice(index, 0, val);
    for (const l of this.loops.values()) {
      if (l.index <= index) {
        l.index++;
      }
    }
    return val;
  }

  public deleteIndex(index: number) {
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
    if (item instanceof FieldSymbol) {
      const p = item.getPointer();
      if (p === undefined) {
        throw new Error("APPEND, fs not assigned");
      }
      this.value.push(p);
      return item;
    } else if (item instanceof DataReference) {
      const ref = new DataReference();
      ref.assign(item.getPointer());
      this.value.push(ref);
      return ref;
    } else {
      const val = this.getValue(item, cloneRow);
      const p = clone(this.rowType);
      p.set(val);
      this.value.push(p);
      return val;
    }
  }

  public appendInitial() {
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
    if (typeof item === "number") {
      return new Integer().set(item);
    } else if (typeof item === "string") {
      return new String().set(item);
    } else {
      return cloneRow === true ? clone(item) : item;
    }
  }

}