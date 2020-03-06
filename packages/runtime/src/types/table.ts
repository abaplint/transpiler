import {INumeric} from "./_numeric";
import {ICharacter} from "./_character";
import {Integer} from "./integer";

export class Table  {
  // eslint-disable-next-line @typescript-eslint/prefer-readonly
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

  public append(item: number | string | INumeric | ICharacter | Table) {
    if (typeof item === "number") {
      this.value.push(new Integer({value: item}));
    } else {
      this.value.push(item);
    }
  }
}