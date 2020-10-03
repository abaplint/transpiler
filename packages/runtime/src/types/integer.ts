import {ICharacter} from "./_character";
import {INumeric} from "./_numeric";

export class Integer implements INumeric {
  private value: number;

  public constructor() {
    this.value = 0;
  }

  public set(value: INumeric | ICharacter | string | number) {
    if (typeof value === "number") {
      this.value = value;
    } else if (typeof value === "string") {
      this.value = parseInt(value, 10);
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

  public clear(): void {
    this.value = 0;
  }

  public get(): number {
    return this.value;
  }
}