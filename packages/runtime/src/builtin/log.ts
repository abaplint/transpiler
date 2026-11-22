import {Float} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {throwError} from "../throw_error";

export function log(input: {val: number | string | ICharacter | INumeric}) {
  let num_in: number | undefined = undefined;
  if (typeof input.val === "number") {
    num_in = input.val;
  } else if (typeof input.val === "string") {
    num_in = parseFloat(input.val);
  } else if (input.val instanceof Float) {
    num_in = input.val.getRaw();
  } else {
    num_in = parseFloat(input.val.get().toString());
  }
  if (num_in < 0) {
    throwError("CX_SY_ARG_OUT_OF_DOMAIN");
  } else if (num_in === 0) {
    throwError("CX_SY_ARITHMETIC_OVERFLOW");
  }
  return Math.log(num_in);
}
