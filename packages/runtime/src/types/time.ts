import {Hex} from "./hex";
import {ICharacter} from "./_character";
import {INumeric} from "./_numeric";

export class Time implements ICharacter {
  private value: string;

  public constructor() {
    this.clear();
  }

  public set(value: INumeric | ICharacter | Hex | string | number) {
    if (typeof value === "number") {
      this.value = this.value.toString();
    } else if (typeof value === "string") {
      this.value = parseInt(value, 10) + "";
    } else {
      this.set(value.get());
    }
    return this;
  }

  public clear(): void {
    this.value = "000000";
  }

  public get(): string {
    return this.value;
  }
}