/* eslint-disable radix */
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function floor(input: {val: number | string | ICharacter | INumeric}) {
  let num_in: number | undefined = undefined;
  if (typeof input.val === "number") {
    num_in = input.val;
  } else if ( typeof input.val === "string") {
    num_in = parseFloat(input.val);
  } else {
    num_in = parseFloat(input.val.get().toString());
  }
  return Math.floor(num_in);
}