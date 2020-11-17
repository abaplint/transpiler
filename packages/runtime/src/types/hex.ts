import {ICharacter} from "./_character";
import {INumeric} from "./_numeric";

export class Hex implements ICharacter {
  private value: string;
  private readonly length: number;

  public constructor(input?: {length?: number}) {
    this.value = "";
    this.length = input?.length ? input?.length : 1;
  }

  public set(value: ICharacter | INumeric | string) {
    if (typeof value === "string") {
      this.value = value;
    } else {
      const v = value.get();
      if (typeof v === "number") {
        this.value = v.toString(16);
        this.value = this.value.padStart(this.length * 2, "0");
      } else {
        this.value = v;
      }
    }
    if (this.value.length > this.length * 2) {
      this.value = this.value.substr(0, this.length * 2);
    }
  }

  public getLength() {
    return this.length;
  }

  public clear(): void {
    this.value = "";
  }

  public get(input?: {offset: number, length: number}): string {
    let ret = this.value;
    if (input?.offset) {
      ret = ret.substr(input.offset * 2);
    }
    if (input?.length) {
      ret = ret.substr(0, input.length * 2);
    }
    return ret;
  }
}