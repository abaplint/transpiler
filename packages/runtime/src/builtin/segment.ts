import {throwError} from "../throw_error";
import {String} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function segment(input: {val: ICharacter | string, index: INumeric | number, sep: ICharacter | string}) {
  let val = input.val;
  if (typeof val !== "string") {
    val = val.get();
  }
  let sep = input.sep;
  if (typeof sep !== "string") {
    sep = sep.get();
  }
  let index = input.index;
  if (typeof index !== "number") {
    index = index.get();
  }

//  const array = val.split(sep);

  if (index === 0) {
    throwError("CX_SY_STRG_PAR_VAL");
  }

  return new String().set("hello");
}