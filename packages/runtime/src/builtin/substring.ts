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
  } else if (off < 0) {
    throwError("CX_SY_RANGE_OUT_OF_BOUNDS");
  }

  const len = input?.len?.get();
  if (len && len < 0) {
    throwError("CX_SY_RANGE_OUT_OF_BOUNDS");
  }

  if (typeof input.val === "string") {
    return new String().set(input.val.substr(off, len));
  } else {
    return input.val.getOffset({offset: off, length: len});
  }
}