import {Hex} from "./hex";
import {XString} from "./xstring";
import {ICharacter} from "./_character";
import {INumeric} from "./_numeric";

export class Integer implements INumeric {
  private value: number;

  public constructor() {
    this.value = 0;
  }

  public set(value: INumeric | ICharacter | Hex | string | number) {
    if (typeof value === "number") {
      this.value = value;
    } else if (typeof value === "string") {
      this.value = parseInt(value, 10);
    } else if (value instanceof Hex || value instanceof XString) {
      this.set(parseInt(value.get(), 16));
    } else {
      this.set(value.get());
    }
    return this;
  }

  public add(value: INumeric | ICharacter | string | number) {
    const t = new Integer();
    t.set(value);
    return new Integer().set(this.value + t.get());
  }

  public minus(value: INumeric | ICharacter | string | number) {
    const t = new Integer();
    t.set(value);
    return new Integer().set(this.value - t.get());
  }

  public multiply(value: INumeric | ICharacter | string | number) {
    const t = new Integer();
    t.set(value);
    return new Integer().set(this.value * t.get());
  }

  public integerDiv(value: INumeric | ICharacter | string | number) {
    const t = new Integer();
    t.set(value);
    return new Integer().set(Math.floor(this.value / t.get()));
  }

  public integerMod(value: INumeric | ICharacter | string | number) {
    const t = new Integer();
    t.set(value);
    return new Integer().set(this.value % t.get());
  }

  public power(value: INumeric | ICharacter | string | number) {
    const t = new Integer();
    t.set(value);
    return new Integer().set(Math.pow(this.value, t.get()));
  }

  public clear(): void {
    this.value = 0;
  }

  public get(): number {
    return this.value;
  }
}