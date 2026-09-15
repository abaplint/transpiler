import {ICharacter} from "../types/_character";
import {String} from "../types/string";
import {INumeric} from "../types/_numeric";
import {throwError} from "../throw_error";
import {position} from "./_position";

export interface ISubstringInput {
  val: ICharacter | string;
  off?: INumeric;
  len?: INumeric;
}

export function substring(input: ISubstringInput): ICharacter {
  let off = position(input?.off);
  if (off === undefined) {
    off = 0;
  } else if (off < 0) {
    throwError("CX_SY_RANGE_OUT_OF_BOUNDS");
  }

  const len = position(input?.len);
  if (len && len < 0) {
    throwError("CX_SY_RANGE_OUT_OF_BOUNDS");
  }

  if (typeof input.val === "string") {
    return new String().set(input.val.substr(off, len));
  } else {
    return input.val.getOffset({offset: off, length: len});
  }
}