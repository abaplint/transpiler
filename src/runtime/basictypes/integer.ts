import {INumeric} from "./_numeric";

export class Integer implements INumeric {
  private value: number;

  public constructor(input?: {value?: number}) {
    this.value = input?.value ? input?.value : 0;
  }

  public set(value: INumeric | number) {
    if (typeof value === "number") {
      this.value = value;
    } else {
      this.value = value.get();
    }
  }

  public add(value: Integer) {
    return new Integer({value: value.value + this.value});
  }

  public equals(value: Integer): boolean {
    return value.value === this.value;
  }

  public get(): number {
    return this.value;
  }
}