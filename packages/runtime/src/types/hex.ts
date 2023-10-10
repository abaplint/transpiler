import {parse} from "../operators/_parse";
import {Float} from "./float";
import {Integer} from "./integer";
import {XString} from "./xstring";
import {ICharacter} from "./_character";
import {INumeric} from "./_numeric";
import {throwError} from "../throw_error";

export class Hex implements ICharacter {
  private value: string;
  private readonly length: number;

  public constructor(input?: {length?: number}) {
    this.length = input?.length ? input?.length : 1;
    this.clear();
  }

  public set(value: ICharacter | INumeric | string | number | Integer | Float) {
    if (typeof value === "string") {
      this.value = value;
    } else if (typeof value === "number") {
      const maxVal = Math.pow(2, this.length * 8);
      if (value < 0) {
        this.value = Math.round(value + maxVal).toString(16);
      } else if (value >= maxVal) {
        const sub = value % maxVal;
        this.value = Math.round(sub).toString(16);
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
    this.value = "0".repeat(this.length * 2);
  }

  public get(): string {
    return this.value;
  }

  public getOffset(input: {offset?: number | INumeric | Hex, length?: number | INumeric | Hex}) {
    let offset = input?.offset;
    if (offset) {
      offset = parse(offset);
    }

    let length = input?.length;
    if (length) {
      length = parse(length);
    }

    if ((offset && offset * 2 > this.value.length)
        || (length && length * 2 > this.value.length)
        || (offset && length && offset * 2 + length * 2 > this.value.length)
        || (offset && offset < 0)
        || (length && length < 0)) {
      throwError("CX_SY_RANGE_OUT_OF_BOUNDS");
    }

    let ret = this.value;
    if (offset) {
      // @ts-ignore
      ret = ret.substr(offset * 2);
    }
    if (length !== undefined) {
      // @ts-ignore
      ret = ret.substr(0, length * 2);
    }
    const r = new XString();
    r.set(ret);
    return r;
  }
}