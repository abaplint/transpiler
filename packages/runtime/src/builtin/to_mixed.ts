/* eslint-disable @typescript-eslint/ban-types */
import {String} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

/*
const UPPER = "U";
const LOWER = "L";
*/

export function to_mixed(input: {
  val: ICharacter | string,
  sep?: ICharacter | string,
  case?: ICharacter | string,
  min?: INumeric | number }): String {

  const sep = "_";
  if (sep.length === 0) {
    throw "CX_SY_STRG_PAR_VAL";
  }
//  const to = UPPER;
  const min = 1;
  if (min < 0) {
    throw "CX_SY_STRG_PAR_VAL";
  }

  let val = input.val;
  if (typeof val !== "string") {
    val = val.get();
  }

  val = val.substring(0, min) + val.substring(min).toLowerCase();

  return new String().set(val);
}