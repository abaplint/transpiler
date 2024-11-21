import {throwError} from "../throw_error";
import {String} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function from_mixed(input: {
  val: ICharacter | string,
  sep?: ICharacter | string,
  case?: ICharacter | string,
  min?: INumeric | number }): String {

  let sep = input.sep;
  if (sep === undefined) {
    sep = "_";
  }
  if (typeof sep !== "string") {
    sep = sep.get();
  }
  if (sep.length === 0) {
    throwError("CX_SY_STRG_PAR_VAL");
  }

  const min = 1;
  if (min < 0) {
    throwError("CX_SY_STRG_PAR_VAL");
  }

  let val = input.val;
  if (typeof val !== "string") {
    val = val.get();
  }

// todo: handle "case" and "min" ?

  const regex = new RegExp(/([A-Z])/, "g");
  val = val.substring(0, 1) + val.substring(1).replace(regex, "_$1");

  return new String().set(val.toUpperCase());
}
