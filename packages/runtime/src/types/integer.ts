import {INumeric} from "./_numeric";

export class Integer implements INumeric {
  private value: number;

  public constructor() {
    this.value = 0;
  }

  public set(value: INumeric | number) {
    if (typeof value === "number") {
      this.value = value;
    } else {
      this.value = value.get();
    }
    return this;
  }

  public add(value: Integer) {
    return new Integer().set(this.value + value.value);
  }

  public minus(value: Integer) {
    return new Integer().set(this.value - value.value);
  }

  public multiply(value: Integer) {
    return new Integer().set(this.value * value.value);
  }

  public clear(): void {
    this.value = 0;
  }

  public get(): number {
    return this.value;
  }
}