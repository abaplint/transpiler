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

  public set(value: ICharacter | INumeric | string | number | Integer | Integer8 | Float | Hex | XString): HexUInt8 {

    let hexString = "";
    if (typeof value === "string") {
      hexString = value;
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
    } else if (value instanceof Integer8) {
      if (value.get() < 0) {
        hexString = (value.get() + 0x10000000000000000n).toString(16).toUpperCase();
      } else {
        hexString = value.get().toString(16).toUpperCase();
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
      }
    }

    if (hexString.length * 2 > this.length) {
      hexString = hexString.substring(0, this.length * 2);
    } else if (hexString.length * 2 < this.length) {
      hexString = hexString.padStart(this.length * 2, "0");
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
// https://stackoverflow.com/questions/39225161/convert-uint8array-into-hex-string-equivalent-in-node-js
// todo: this truncates the value if more than 255?
    return Buffer.from(this.value).toString("hex").toUpperCase();
  }

  public getOffset(input: {offset?: number | INumeric | Hex | Integer8, length?: number | INumeric | Hex | Integer8}): XString {
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

    if (offset !== undefined && length !== undefined) {
      if (offset * 2 + length * 2 > this.value.length) {
        throwError("CX_SY_RANGE_OUT_OF_BOUNDS");
      }
    }

    const str = Buffer.from(this.value, offset, length).toString("hex").toUpperCase();
    return new XString().set(str);
  }
}