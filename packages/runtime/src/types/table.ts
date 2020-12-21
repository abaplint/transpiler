import {INumeric} from "./_numeric";
import {ICharacter} from "./_character";
import {Integer} from "./integer";
import {ABAPObject} from "./abap_object";
import {String} from "./string";
import {clone} from "../clone";
import {Structure} from "./structure";

export type TableRowType = INumeric | Structure | ICharacter | Table | ABAPObject | string | number;

export class Table  {
  private value: any[];
  private readonly rowType: TableRowType;

  public constructor(rowType: TableRowType) {
    this.value = [];
    this.rowType = rowType;
  }

  // Modifications to the array must be done inside this class, in order to keep track of LOOP indexes
  public array(): any[] {
    return this.value;
  }

  public clear(): void {
    this.value = [];
  }

  public set(tab: Table) {
    this.clear();
    for (const a of tab.array()) {
      this.append(a);
    }
  }

  public insertIndex(item: TableRowType, index: number) {
    this.value.splice(index, 0, clone(item));
  }

  public deleteIndex(index: number) {
    this.value.splice(index, 1);
  }

  public append(item: TableRowType, cloneRow = true) {
    if (typeof item === "number") {
      this.value.push(new Integer().set(item));
    } else if (typeof item === "string") {
      this.value.push(new String().set(item));
    } else if (item instanceof ABAPObject) {
      this.value.push(item);
    } else {
      this.value.push(cloneRow === true ? clone(item) : item);
    }
  }

  public appendInitial() {
    // note that this will clone the object
    this.append(this.rowType);
    // return "field symbol" pointing to the inserted line
    return this.value[this.value.length - 1];
  }

}