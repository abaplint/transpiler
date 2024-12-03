import {throwError} from "../throw_error";
import {String} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function to_mixed(input: {
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
  val = val.substring(0, min) + val.substring(min).toLowerCase();

  if (input.case) {
    if (typeof input.case === "string") {
      if (input.case === input.case.toLowerCase()) {
        val = val.substring(0, 1).toLowerCase() + val.substring(1);
      }
    } else {
      if (input.case.get() === input.case.get().toLowerCase()) {
        val = val.substring(0, 1).toLowerCase() + val.substring(1);
      }
    }
  }

  const length = sep.length;
  const regex = new RegExp(sep + "\\w");
  while (val.match(regex)) {
    val = val.replace(regex, (x) => {
      return x.substring(length).toUpperCase();
    });
  }

  return new String().set(val);
}
