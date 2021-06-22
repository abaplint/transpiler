import {Hex, Integer, XString} from "./types";
import {ICharacter} from "./types/_character";
import {INumeric} from "./types/_numeric";

export interface IOffsetLengthOptions {
  length?: INumeric | number,
  offset?: INumeric | number,
}

export class OffsetLength {
  private readonly obj: ICharacter;
  private readonly offset?: number;
  private readonly length?: number;
  private readonly isHex: boolean;

  public constructor(obj: ICharacter, options: IOffsetLengthOptions) {
    this.obj = obj;

    this.isHex = obj instanceof Hex || obj instanceof XString;

    if (options.offset) {
      if (typeof options.offset === "number") {
        this.offset = options.offset;
      } else {
        this.offset = options.offset.get();
      }
      if (this.isHex) {
        this.offset *= 2;
      }
    }

    if (options.length) {
      if (typeof options.length === "number") {
        this.length = options.length;
      } else {
        this.length = options.length.get();
      }
      if (this.isHex) {
        this.length *= 2;
      }
    }
  }

  public set(value: ICharacter | string) {

    let val = "";
    if (typeof value === "string") {
      val = value;
    } else if (typeof value === "number") {
      val = value + "";
    } else if (value instanceof Integer) {
      val = value.get() + "";
      if (this.isHex) {
        val = Number(val).toString(16);
      }
    } else {
      val = value.get() + "";
    }

    let old = this.obj.get();

    if (this.length) {
      val = val.substr(0, this.length);
      if (this.isHex) {
        val = val.padStart(this.length, "0");
      }
    }

    if (this.length && this.offset) {
      old = old.substr(0, this.offset) + val + old.substr(this.offset + this.length);
    } else if (this.length) {
      old = val + old.substr(this.length);
    } else if (this.offset) {
      old = old.substr(0, this.offset) + val;
    }

    this.obj.set(old);
  }
}