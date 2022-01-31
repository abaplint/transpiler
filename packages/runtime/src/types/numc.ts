import {Hex} from "./hex";
import {ICharacter} from "./_character";
import {INumeric} from "./_numeric";

export class Numc implements ICharacter {
  private value: string;
  private readonly length: number;

  public constructor(input?: {length?: number}) {
    this.length = input?.length ? input?.length : 1;
    this.clear();
  }

  public set(value: INumeric | ICharacter | Hex | string | number) {
    if (typeof value === "number") {
      this.value = value.toString();
    } else if (typeof value === "string") {
      this.value = parseInt(value, 10) + "";
    } else {
      this.set(value.get());
      return;
    }

    if (this.value.length > this.length) {
      this.value = this.value.substr(this.value.length - this.length, this.length);
    } else {
      const pad = this.length - this.value.length;
      if (pad > 0) {
        this.value = "0".repeat(pad) + this.value;
      }
    }

    return this;
  }

  public getLength() {
    return this.length;
  }

  public clear(): void {
    this.value = "0".repeat(this.length);
  }

  public get(): string {
    return this.value;
  }
}