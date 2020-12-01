import {Hex} from "./hex";
import {XString} from "./xstring";
import {ICharacter} from "./_character";
import {INumeric} from "./_numeric";

export class Numc implements ICharacter {
  private value: number;
  private readonly length: number;

  public constructor(input?: {length?: number}) {
    this.value = 0;
    this.length = input?.length ? input?.length : 1;
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

  public clear(): void {
    this.value = 0;
  }

  public get(): string {
    let ret = this.value.toString();
    const pad = this.length - ret.length;
    ret = "0".repeat(pad) + ret;
    return ret;
  }
}