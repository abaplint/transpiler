import {Hex} from "./hex";
import {parse} from "../operators/_parse";
import {ICharacter} from "./_character";
import {INumeric} from "./_numeric";
import {throwError} from "../throw_error";
import {Float} from "./float";
import {DecFloat34} from "./decfloat34";

const initialValues: {[length: number]: string} = {};
const nonDigits = /\D/g;

export class Numc implements ICharacter {
  private value: string;
  private readonly length: number;
  private readonly qualifiedName: string | undefined;

  public constructor(input?: {length?: number, qualifiedName?: string}) {
    this.length = input?.length ? input?.length : 1;
    this.qualifiedName = input?.qualifiedName;
    this.clear();
  }

  public clone(): Numc {
    const n = new Numc({length: this.length, qualifiedName: this.qualifiedName});
    // set without trigger checks and padding
    n.value = this.value;
    return n;
  }

  public getQualifiedName() {
    return this.qualifiedName;
  }

  public set(value: INumeric | ICharacter | Hex | string | number, raw = false) {
    if (value instanceof Numc && value.getLength() === this.length) {
      this.value = value.get();
      return this;
    } else if (typeof value === "number") {
      this.value = Math.trunc(value) + "";
    } else if (typeof value === "string") {
      // every digit is kept, everything else is dropped, as text: no precision limit
      const digits = value.replace(nonDigits, "");
      if (digits === "") {
        this.clear();
        return this;
      }
      this.value = digits;
    } else if (value instanceof DecFloat34) {
      // get() is text with a decimal comma, only the integer part counts
      this.set(String(value.get()).split(",")[0]);
      return this;
    } else if (value instanceof Float) {
      this.value = Math.trunc(value.getRaw()) + "";
    } else {
      const inner = value.get();
      if (typeof inner !== "bigint") {
        this.set(inner);
        return this;
      }
      // int8: the absolute value, digit by digit, a number would lose precision above 2^53
      this.value = (inner < 0n ? -inner : inner).toString();
    }

    if (this.value.length > this.length) {
      this.value = this.value.substr(this.value.length - this.length, this.length);
    } else {
      const pad = this.length - this.value.length;
      if (pad > 0 && raw === false) {
        this.value = "0".repeat(pad) + this.value;
      }
    }

    return this;
  }

  public getLength() {
    return this.length;
  }

  public clear(): void {
    if (initialValues[this.length] === undefined) {
      initialValues[this.length] = "0".repeat(this.length);
    }
    this.value = initialValues[this.length];
  }

  public get(): string {
    return this.value;
  }

  public getOffset(input: {offset?: number | INumeric | Hex, length?: number | INumeric | Hex}): Numc {
    let offset = input?.offset;
    if (offset) {
      offset = parse(offset);
    }

    let length = input?.length;
    if (length) {
      length = parse(length);
    }

    if ((offset && offset >= this.length)
         || (offset && offset < 0)
         || (length && length < 0)) {
      throwError("CX_SY_RANGE_OUT_OF_BOUNDS");
    }

    let ret = this.value;
    if (offset) {
      ret = ret.substr(offset);
    }
    if (length !== undefined) {
      ret = ret.substr(0, length);
    }
    const r = new Numc({length: ret.length});
    r.set(ret);
    return r;
  }
}