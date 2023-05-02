import {throwError} from "../throw_error.js";
import {String} from "../types/index.js";
import {ICharacter} from "../types/_character.js";
import {INumeric} from "../types/_numeric.js";

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

  if (index === 0 || sep.length === 0) {
    throwError("CX_SY_STRG_PAR_VAL");
  }

  const array = val.split(sep);

  if (index < 0) {
    array.reverse();
    index = Math.abs( index );
  }

  if (index > array.length) {
    throwError("CX_SY_STRG_PAR_VAL");
  }

  return new String().set(array[index - 1]);
}