import {parse} from "../operators/_parse";
import {Float} from "./float";
import {Integer} from "./integer";
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

  public set(value: ICharacter | INumeric | string | number | Integer | Float) {
    if (typeof value === "string") {
      this.value = value;
    } else if (typeof value === "number") {
      if (value < 0) {
        const maxVal = Math.pow(2, this.length * 8);
        this.value = Math.round(value + maxVal).toString(16);
      } else {
        this.value = Math.round(value).toString(16);
      }
      this.value = this.value.padStart(this.length * 2, "0");
    } else {
      let v = value.get();
      if (value instanceof Float) {
        v = value.getRaw();
        this.set(v);
      } else if (typeof v === "number") {
        this.set(v);
      } else {
        this.value = v;
        if (this.value.match(/^(?![A-F0-9])/)) {
          this.value = "";
        }
      }
    }
    if (this.value.length > this.length * 2) {
      this.value = this.value.substr(0, this.length * 2);
    }
    if (this.value.length < this.length * 2) {
      this.value = this.value.padEnd(this.length * 2, "0");
    }
    this.value = this.value.toUpperCase();
    return this;
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

  public getOffset(input: {offset?: number | INumeric | Hex, length?: number | INumeric | Hex}) {
    if (input?.offset) {
      input.offset = parse(input.offset);
    }
    if (input?.length) {
      input.length = parse(input.length);
    }
    let ret = this.value;
    if (input?.offset) {
      // @ts-ignore
      ret = ret.substr(input.offset * 2);
    }
    if (input?.length !== undefined) {
      // @ts-ignore
      ret = ret.substr(0, input.length * 2);
    }
    const r = new XString();
    r.set(ret);
    return r;
  }
}