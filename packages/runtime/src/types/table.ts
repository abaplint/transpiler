import {INumeric} from "./_numeric";
import {ICharacter} from "./_character";
import {Integer} from "./integer";
import {ABAPObject} from "./abap_object";
import {String} from "./string";
import {clone} from "../clone";

export class Table  {
  private value: any[];
  private readonly rowType: INumeric | ICharacter | Table | ABAPObject;

  public constructor(rowType: INumeric | ICharacter | Table | ABAPObject) {
    this.value = [];
    this.rowType = rowType;
  }

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

  public append(item: number | string | INumeric | ICharacter | Table | ABAPObject) {
    if (typeof item === "number") {
      this.value.push(new Integer().set(item));
    } else if (typeof item === "string") {
      this.value.push(new String().set(item));
    } else if (item instanceof ABAPObject) {
      this.value.push(item);
    } else {
      this.value.push(clone(item));
    }
  }

  public appendInitial() {
    // note that this will clone the object
    this.append(this.rowType);
    // return "field symbol" pointing to the inserted line
    return this.value[this.value.length - 1];
  }

}