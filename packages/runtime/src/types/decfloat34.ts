import {Float} from ".";
import {Hex} from "./hex";
import {XString} from "./xstring";
import {ICharacter} from "./_character";
import {INumeric} from "./_numeric";

export class DecFloat34 {
  private value: number;

  public constructor() {
    this.value = 0;
  }

  public clone(): DecFloat34 {
    const n = new DecFloat34();
    n.value = this.value;
    return n;
  }

  public set(value: INumeric | ICharacter | Hex | string | number) {
    if (typeof value === "number") {
      this.value = value;
    } else if (typeof value === "string" && value.trim().length === 0) {
      this.value = 0;
    } else if (typeof value === "string") {
      this.value = parseFloat(value);
    } else if (value instanceof Float) {
      this.value = value.getRaw();
    } else if (value instanceof Hex || value instanceof XString) {
// todo, how/if should this work?
      this.set(parseInt(value.get(), 16));
    } else {
      this.set(value.get());
    }
    return this;
  }

  public clear(): void {
    this.value = 0;
  }

  public getRaw(): number {
    return this.value;
  }

  public get(): string {
    let text = new Number(this.value).toString();
    text = text.replace(".", ",");
    return text;
  }
}