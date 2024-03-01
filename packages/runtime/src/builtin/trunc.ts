/* eslint-disable radix */
import {Float} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function trunc(input: {val: number | string | ICharacter | INumeric}) {
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
  return Math.trunc(num_in);
}