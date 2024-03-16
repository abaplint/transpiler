import {parse} from "../operators/_parse";
import {Float} from "./float";
import {Integer} from "./integer";
import {XString} from "./xstring";
import {ICharacter} from "./_character";
import {INumeric} from "./_numeric";
import {throwError} from "../throw_error";
import {Integer8} from "./integer8";

const REGEXP = /^(?![A-F0-9])/;

export class Hex implements ICharacter {
  private value: string;
  private readonly length: number;
  private readonly qualifiedName: string | undefined;

  public constructor(input?: {length?: number, qualifiedName?: string}) {
    this.length = input?.length ? input?.length : 1;
    this.qualifiedName = input?.qualifiedName;
    this.clear();
  }

  public getQualifiedName() {
    return this.qualifiedName;
  }

  public set(value: ICharacter | INumeric | string | number | Integer | Integer8 | Float | Hex | XString): Hex {
    const doubleLength = this.length * 2;
    if (typeof value === "string") {
      this.value = value;
    } else if (typeof value === "number") {
      const maxVal = Math.pow(2, this.length * 8);
      if (value < 0) {
        let hex = Math.round(value + 0x100000000).toString(16).toUpperCase();
        if (hex.length > doubleLength) {
          hex = hex.substring(hex.length - doubleLength);
        }
        this.value = hex;
      } else if (value >= maxVal) {
        const sub = value % maxVal;
        this.value = Math.round(sub).toString(16).toUpperCase();
      } else {
        this.value = Math.round(value).toString(16).toUpperCase();
      }
      this.value = this.value.padStart(doubleLength, "0");
    } else if (value instanceof Integer8) {
      let hex = "";
      if (value.get() < 0) {
        hex = (value.get() + 0x10000000000000000n).toString(16).toUpperCase();
        if (hex.length > doubleLength) {
          hex = hex.substring(hex.length - doubleLength);
        }
      } else {
        hex = value.get().toString(16).toUpperCase();
        hex = hex.padStart(doubleLength, "0");
      }
      return this.set(hex);
    } else if (value instanceof Hex) {
      this.value = value.get();
    } else {
      const v = value.get();
      if (value instanceof Float) {
        return this.set(value.getRaw());
      } else if (typeof v === "number") {
        return this.set(v);
      } else {
        this.value = v;
        if (this.value.match(REGEXP)) {
          this.value = "";
        }
      }
    }

    if (this.value.length > doubleLength) {
      this.value = this.value.substr(0, doubleLength);
    } else if (this.value.length < doubleLength) {
      this.value = this.value.padEnd(doubleLength, "0");
    }
//    this.value = this.value.toUpperCase(); // todo, for some reason abapNTLM needs this? investigate
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
      throw new Error("hex: getOffset, unexpected");
    }
  }
}