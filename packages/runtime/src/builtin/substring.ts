import {ICharacter} from "../types/_character";
import {String} from "../types/string";
import {INumeric} from "../types/_numeric";
import {throwError} from "../throw_error";

export interface ISubstringInput {
  val: ICharacter | string;
  off?: INumeric;
  len?: INumeric;
}

export function substring(input: ISubstringInput): ICharacter {
  let off = input?.off?.get();
  if (off === undefined) {
    off = 0;
  }
  if (off < 0) {
    throwError("CX_SY_RANGE_OUT_OF_BOUNDS");
  }

  const len = input?.len?.get();
  if (len && len < 0) {
    throwError("CX_SY_RANGE_OUT_OF_BOUNDS");
  }

  let sub = "";
  if (typeof input.val === "string") {
    sub = input.val.substr(off, len);
  } else {
    sub = input.val.getOffset({offset: off, length: len}).get();
  }
  return new String().set(sub);
}