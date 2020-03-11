import {INumeric} from "./_numeric";
import {ICharacter} from "./_character";
import {Integer} from "./integer";
import {ABAPObject} from "./abap_object";
import {String} from "./string";

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
    } else if (typeof item === "string") {
      this.value.push(new String().set(item));
    } else {
      this.value.push(item);
    }
  }
}