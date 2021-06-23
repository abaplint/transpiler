import {minus} from "../operators";
import {Float} from "./float";
import {XString} from "./xstring";
import {ICharacter} from "./_character";
import {INumeric} from "./_numeric";

export class Hex implements ICharacter {
  private value: string;
  private readonly length: number;

  public constructor(input?: {length?: number}) {
    this.length = input?.length ? input?.length : 1;
    this.value = "0".repeat(this.length * 2);
  }

  public set(value: ICharacter | INumeric | string | number) {
    if (typeof value === "string") {
      this.value = value;
    } else if (typeof value === "number") {
      this.value = Math.round(value).toString(16);
      this.value = this.value.padStart(this.length * 2, "0");
    } else {
      let v = value.get();
      if (value instanceof Float) {
        v = value.getRaw();
        if (v < 0) {
          this.value = "F".repeat(this.length * 2);
          this.set(minus(this, Math.abs(v) - 1));
          return;
        }
      }
      if (typeof v === "number") {
        this.value = Math.round(v).toString(16);
        this.value = this.value.padStart(this.length * 2, "0");
      } else {
        this.value = v;
      }
    }
    if (this.value.length > this.length * 2) {
      this.value = this.value.substr(0, this.length * 2);
    }
    this.value = this.value.toUpperCase();
  }

  public getLength() {
    return this.length;
  }

  public clear(): void {
    this.value = "";
  }

  public get(): string {
    return this.value;
  }

  public getOffset(input: {offset: number, length: number}) {
    let ret = this.value;
    if (input?.offset) {
      ret = ret.substr(input.offset * 2);
    }
    if (input?.length !== undefined) {
      ret = ret.substr(0, input.length * 2);
    }
    const r = new XString();
    r.set(ret);
    return r;
  }
}