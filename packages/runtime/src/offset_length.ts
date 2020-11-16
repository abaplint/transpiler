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

  public constructor(obj: ICharacter, options: IOffsetLengthOptions) {
    this.obj = obj;

    if (options.offset) {
      if (typeof options.offset === "number") {
        this.offset = options.offset;
      } else {
        this.offset = options.offset.get();
      }
    }

    if (options.length) {
      if (typeof options.length === "number") {
        this.length = options.length;
      } else {
        this.length = options.length.get();
      }
    }
  }

  public set(value: ICharacter | string) {

    let val = "";
    if (typeof value === "string" || typeof value === "number") {
      val = value;
    } else {
      val = value.get();
    }

    let old = this.obj.get();
    if (this.length && this.offset) {
      old = old.substr(0, this.offset) + val.substr(0, this.length) + old.substr(this.offset + this.length);
    } else if (this.length) {
      old = val.substr(0, this.length) + old.substr(this.length);
    } else if (this.offset) {
      old = old.substr(0, this.offset) + val;
    }

    this.obj.set(old);
  }
}