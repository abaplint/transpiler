import {INumeric} from "./_numeric";

export class Packed implements INumeric {
  private value: number;

  public constructor(input?: {value?: number, length?: number, decimals?: number}) {
    this.value = input?.value ? input?.value : 0;
// todo, length and decimals
  }

  public set(value: INumeric | number) {
    if (typeof value === "number") {
      this.value = value;
    } else {
      this.value = value.get();
    }
  }

  public add(_value: INumeric) {
// todo
//    return new Packed({value: value.value + this.value});
  }

  public clear(): void {
    this.value = 0;
  }

  public equals(value: INumeric): boolean {
    return value.get() === this.value;
  }

  public get(): number {
    return this.value;
  }
}