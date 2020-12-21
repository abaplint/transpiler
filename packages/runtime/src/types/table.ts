import {INumeric} from "./_numeric";
import {ICharacter} from "./_character";
import {Integer} from "./integer";
import {ABAPObject} from "./abap_object";
import {String} from "./string";
import {clone} from "../clone";
import {Structure} from "./structure";

export class LoopIndex {
  public index: number;

  public constructor(start: number) {
    this.index = start;
  }
}

export type TableRowType = INumeric | Structure | ICharacter | Table | ABAPObject | string | number;

export class Table  {
  private value: TableRowType[];
  private readonly rowType: TableRowType;
  private readonly loops: Set<LoopIndex>;

  public constructor(rowType: TableRowType) {
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
    this.value.splice(index, 0, this.getValue(item));
  }

  public deleteIndex(index: number) {
    this.value.splice(index, 1);
  }

  public append(item: TableRowType, cloneRow = true) {
    this.value.push(this.getValue(item, cloneRow));
  }

  public appendInitial() {
    // note that this will clone the object
    this.append(this.rowType);
    // return "field symbol" pointing to the inserted line
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
    } else if (item instanceof ABAPObject) {
      return item;
    } else {
      return cloneRow === true ? clone(item) : item;
    }
  }

}