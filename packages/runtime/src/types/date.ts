import {Hex} from "./hex";
import {ICharacter} from "./_character";
import {INumeric} from "./_numeric";

export class Date implements ICharacter {
  private value: string;

  public constructor() {
    this.clear();
  }

  public set(value: INumeric | ICharacter | Hex | string | number) {
    if (typeof value === "number") {
      this.value = value.toString().padStart(8,"0");
    } else if (typeof value === "string") {
      this.value = value;
    } else {
      this.set(value.get());
    }
    return this;
  }

  public clear(): void {
    this.value = "00000000";
  }

  public get(): string {
    return this.value;
  }
}