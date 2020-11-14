import {INumeric} from "./_numeric";
import {ICharacter} from "./_character";
import {Integer} from "./integer";
import {ABAPObject} from "./abap_object";
import {String} from "./string";
import {Structure} from "./structure";
import {clone} from "../clone";

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
    } else if (item instanceof Table) {
      this.value.push(clone(item));
    } else if (item instanceof Structure) {
      this.value.push(clone(item));
    } else if (item instanceof ABAPObject) {
      this.value.push(item);
    } else {
      this.append(item.get());
    }
  }
}