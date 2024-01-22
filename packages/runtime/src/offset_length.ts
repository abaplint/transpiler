import {Character, Hex, Integer, Structure, Time, XString} from "./types";
import {ICharacter} from "./types/_character";
import {INumeric} from "./types/_numeric";

export interface IOffsetLengthOptions {
  length?: INumeric | number,
  offset?: INumeric | number,
}

export class OffsetLength {
  private readonly obj: ICharacter | Character | Hex | XString | Structure;
  private readonly offset?: number;
  private readonly length?: number;
  private readonly isHex: boolean;

  public constructor(obj: ICharacter | Character | Hex | XString | Structure, options: IOffsetLengthOptions) {
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

  public get() {
    if (this.isHex) {
      let offset = this.offset;
      if (offset) {
        offset = offset / 2;
      }
      let length = this.length;
      if (length) {
        length = length / 2;
      }
      return this.obj.getOffset({offset: offset, length: length}).get();
    } else {
      return this.obj.getOffset({offset: this.offset, length: this.length}).get();
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

    let old = this.obj instanceof Structure ? this.obj.getCharacter() : this.obj.get();

    if (this.obj instanceof Character) {
      old = old.padEnd(this.obj.getLength(), " ");
    }

    if (this.length) {
      val = val.substr(0, this.length);
      if (this.isHex || this.obj instanceof Time) {
        val = val.padStart(this.length, "0");
      } else if (val.length < this.length) {
        val = val.padEnd(this.length, " ");
      }
    }

    if (this.length && this.offset) {
      old = old.substr(0, this.offset) + val + old.substr(this.offset + this.length);
    } else if (this.length) {
      old = val + old.substr(this.length);
    } else if (this.offset) {
      old = old.substr(0, this.offset) + val;
    }
    old = old.trimEnd();

    if (this.obj instanceof Character) {
      old = old.padEnd(this.obj.getLength(), " ");
    }

    this.obj.set(old);
  }
}