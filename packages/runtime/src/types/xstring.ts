import {parse} from "../operators/_parse";
import {Float} from "./float";
import {Hex} from "./hex";
import {ICharacter} from "./_character";
import {INumeric} from "./_numeric";
import {Character} from "./character";
import {throwError} from "../throw_error";
import {Integer} from "./integer";
import {Integer8} from "./integer8";
import {Packed} from "./packed";

export class XString implements ICharacter {
  private value: string;
  private readonly qualifiedName: string | undefined;

  public constructor(input?: {qualifiedName?: string}) {
    this.value = "";
    this.qualifiedName = input?.qualifiedName;
  }

  public clone(): XString {
    const n = new XString({qualifiedName: this.qualifiedName});
    n.value = this.value;
    return n;
  }

  public getQualifiedName() {
    return this.qualifiedName;
  }

  public set(value: ICharacter | INumeric | string | number | Integer8) {
    if (typeof value === "string") {
      // the input is interpreted as hexadecimal digits, the conversion stops at
      // the first character which is not an uppercase hex digit
      this.value = /^[0-9A-F]*/.exec(value)?.[0] || "";
      if (this.value.length % 2 === 1) {
        this.value = this.value + "0";
      }
    } else if (typeof value === "number") {
      this.value = Math.round(value).toString(16).toUpperCase();
      if (this.value.length % 2 === 1) {
        this.value = "0" + this.value;
      }
    } else if (value instanceof Integer8) {
      const v: bigint = (value as Integer8).get();
      if (v < 0n) {
        // two's complement, as int8 -> x LENGTH 8
        this.value = new Hex({length: 8}).set(value).get();
      } else {
        this.value = v.toString(16).toUpperCase();
        if (this.value.length % 2 === 1) {
          this.value = "0" + this.value;
        }
      }
    } else if (value instanceof Integer && value.get() < 0) {
      // two's complement, as i -> x LENGTH 4
      this.value = (value.get() + 0x100000000).toString(16).toUpperCase();
    } else if (value instanceof XString || value instanceof Hex) {
      // already hexadecimal digits, uppercase and an even number of them
      this.value = value.get();
    } else {
      let v = value.get();
      if (value instanceof Float) {
        v = value.getRaw();
        this.set(v);
      } else if (value instanceof Packed) {
        // rounded as f is, the number is not written with its decimals
        this.set(value.get());
      } else if (value instanceof Character) {
        this.set(value.getTrimEnd());
      } else if (typeof v === "number") {
        this.value = v.toString(16).toUpperCase();
        if (this.value.length % 2 === 1) {
          this.value = "0" + this.value;
        }
      } else {
        this.set(v);
      }
    }
    return this;
  }

  /** sets hexadecimal digits the caller knows to be valid: uppercase 0-9 and A-F, an even
   *  number of them. set( ) checks a string first, which takes time in its length */
  public setHex(value: string): XString {
    this.value = value;
    return this;
  }

  public clear(): void {
    this.value = "";
  }

  public get(): string {
    return this.value;
  }

  public getOffset(input: {offset?: number | INumeric | Hex | Integer8, length?: number | INumeric | Hex | Integer8}) {
    let offset = input?.offset;
    if (offset) {
      if (offset instanceof Integer8) {
        offset = Number(offset.get());
      } else {
        offset = parse(offset);
      }
      if (offset * 2 > this.value.length
          || offset < 0) {
        throwError("CX_SY_RANGE_OUT_OF_BOUNDS");
      }
    }

    let length = input?.length;
    if (length) {
      if (length instanceof Integer8) {
        length = Number(length.get());
      } else {
        length = parse(length);
      }
      if (length * 2 > this.value.length
          || length < 0) {
        throwError("CX_SY_RANGE_OUT_OF_BOUNDS");
      }
    }

    // NOTE: this only copies the minimal length of the string,
    if (offset !== undefined && length !== undefined) {
      if (offset * 2 + length * 2 > this.value.length) {
        throwError("CX_SY_RANGE_OUT_OF_BOUNDS");
      }
      return new XString().set(this.value.substr(offset * 2, length * 2));
    } else if (offset !== undefined) {
      return new XString().set(this.value.substr(offset * 2));
    } else if (length !== undefined) {
      return new XString().set(this.value.substr(0, length * 2));
    } else {
      throw new Error("xstring: getOffset, unexpected");
    }
  }
}