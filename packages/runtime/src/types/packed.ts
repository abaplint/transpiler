import {INumeric} from "./_numeric";

export class Packed implements INumeric {
  private value: number;
  private readonly length: number;
  private readonly decimals: number;

  public constructor(input?: {length?: number, decimals?: number}) {
    this.value = 0;

    this.length = 666;
    if (input?.length) {
      this.length = input.length;
    }

    this.decimals = 666;
    if (input?.decimals) {
      this.decimals = input.decimals;
    }
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
    console.log(this.length);
    console.log(this.decimals);
  }

  public clear(): void {
    this.value = 0;
  }

  public get(): number {
    return this.value;
  }
}