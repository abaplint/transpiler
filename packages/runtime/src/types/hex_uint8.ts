/* eslint-disable no-bitwise */
import {parse} from "../operators/_parse";
import {Float} from "./float";
import {Integer} from "./integer";
import {XString} from "./xstring";
import {ICharacter} from "./_character";
import {INumeric} from "./_numeric";
import {throwError} from "../throw_error";
import {Integer8} from "./integer8";
import {Hex} from "./hex";

const REGEXP = /^(?![A-F0-9])/;

const LUT_HEX_4b = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F"];
const LUT_HEX_8b = new Array(0x100);
for (let n = 0; n < 0x100; n++) {
  LUT_HEX_8b[n] = `${LUT_HEX_4b[(n >>> 4) & 0xF]}${LUT_HEX_4b[n & 0xF]}`;
}

export class HexUInt8 implements ICharacter {
  private value: Uint8Array;
  private readonly length: number;
  private readonly qualifiedName: string | undefined;

  public constructor(input?: {length?: number, qualifiedName?: string}) {
    this.length = input?.length ? input?.length : 1;
    this.qualifiedName = input?.qualifiedName;
    this.value = new Uint8Array(this.length);
  }

  public getQualifiedName() {
    return this.qualifiedName;
  }

  public setOffset(offset: number, value: number) {
// caller must validate offset
    this.value[offset] = value;
  }

  public getOffsetRaw(offset: number) {
// caller must validate offset
    return this.value[offset];
  }

  public set(value: ICharacter | INumeric | string | number | Integer | Integer8 | Float | Hex | XString): HexUInt8 {
    let hexString = "";
    if (typeof value === "string") {
      hexString = value;
      if (hexString.length < this.length * 2) {
        hexString = hexString.padEnd(this.length * 2, "0");
      }
    } else if (typeof value === "number") {
      const maxVal = Math.pow(2, this.length * 8);
      if (value < 0) {
        hexString = Math.round(value + 0x100000000).toString(16).toUpperCase();
      } else if (value >= maxVal) {
        const sub = value % maxVal;
        hexString = Math.round(sub).toString(16).toUpperCase();
      } else {
        hexString = Math.round(value).toString(16).toUpperCase();
      }
      if (hexString.length > this.length * 2) {
        hexString = hexString.substring(hexString.length - this.length * 2);
      } else if (hexString.length < this.length * 2) {
        hexString = hexString.padStart(this.length * 2, "0");
      }
    } else if (value instanceof Integer8) {
      if (value.get() < 0) {
        hexString = (value.get() + 0x10000000000000000n).toString(16).toUpperCase();
      } else {
        hexString = value.get().toString(16).toUpperCase();
      }
      if (hexString.length > this.length * 2) {
        hexString = hexString.substring(hexString.length - this.length * 2);
      } else if (hexString.length < this.length * 2) {
        hexString = hexString.padStart(this.length * 2, "0");
      }
    } else if (value instanceof HexUInt8 || value instanceof XString) {
      hexString = value.get();
      if (hexString.length < this.length * 2) {
        hexString = hexString.padEnd(this.length * 2, "0");
      }
    } else {
      const v = value.get();
      if (value instanceof Float) {
        return this.set(value.getRaw());
      } else if (typeof v === "number") {
        return this.set(v);
      } else {
        hexString = v;
        if (hexString.match(REGEXP)) {
          hexString = "";
        }
        if (hexString.length < this.length * 2) {
          hexString = hexString.padEnd(this.length * 2, "0");
        }
      }
    }

    if (hexString.length > this.length * 2) {
      hexString = hexString.substring(0, this.length * 2);
    }
    this.value = Uint8Array.from(Buffer.from(hexString, "hex"));

    return this;
  }

  public getLength() {
    return this.length;
  }

  public clear(): void {
// optimize? https://gist.github.com/chrisj/872283d15e1bb460a4766a52f50ebcf6
    for (let i = 0; i < this.value.length; i++) {
      this.value[i] = 0;
    }
  }

  public get(): string {
//    return Buffer.from(this.value).toString("hex").toUpperCase();
    let out = "";
    for (let idx = 0, edx = this.value.length; idx < edx; idx++) {
      out += LUT_HEX_8b[this.value[idx]];
    }
    return out;
  }

  public getOffset(input: {offset?: number | INumeric | Hex | Integer8, length?: number | INumeric | Hex | Integer8}): XString {
    let offset = input?.offset;
    if (offset) {
      if (offset instanceof Integer8) {
        offset = Number(offset.get());
      } else {
        offset = parse(offset);
      }
      if (offset > this.length
          || offset < 0) {
        throwError("CX_SY_RANGE_OUT_OF_BOUNDS");
      }
    } else {
      offset = 0;
    }

    let length = input?.length;
    if (length) {
      if (length instanceof Integer8) {
        length = Number(length.get());
      } else {
        length = parse(length);
      }
      if (length > this.length
          || length < 0) {
        throwError("CX_SY_RANGE_OUT_OF_BOUNDS");
      }
    }

    if (offset !== undefined && length !== undefined) {
      if (offset + length > this.length) {
        throwError("CX_SY_RANGE_OUT_OF_BOUNDS");
      }
    }

    // not sure how this works: without copying, https://nodejs.org/api/buffer.html#static-method-bufferfromarraybuffer-byteoffset-length
/*
    console.dir(offset);
    console.dir(length);
    console.dir(this.value.subarray(offset, length ? offset + length : undefined));
    console.dir(Buffer.from(this.value.subarray(offset, length ? offset + length : undefined)));
*/
    const str = Buffer.from(this.value.subarray(offset, length ? offset + length : undefined)).toString("hex")
      .toUpperCase();
    return new XString().set(str);
  }
}