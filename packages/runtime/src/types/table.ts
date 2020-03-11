import {INumeric} from "./_numeric";
import {ICharacter} from "./_character";
import {Integer} from "./integer";
import {ABAPObject} from "./abap_object";

export class Table  {
  private value: any[];

  public constructor() {
    this.value = [];
  }

  public array(): any[] {
    return this.value;
  }

  public clear(): void {
    this.value = [];
  }

  public append(item: number | string | INumeric | ICharacter | Table | ABAPObject) {
    if (typeof item === "number") {
      this.value.push(new Integer().set(item));
    } else {
      this.value.push(item);
    }
  }
}