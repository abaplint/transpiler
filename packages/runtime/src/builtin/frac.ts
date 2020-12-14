/* eslint-disable radix */
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function frac(input: number | string | ICharacter | INumeric) {
  let num_in: number | undefined = undefined;
  let ret = 0;
  let pre = "0.";
  if (typeof input === "number") {
    num_in = input;
  } else if ( typeof input === "string") {
    num_in = parseFloat(input);
  } else {
    num_in = parseFloat(input.get().toString());
  }
  const numSplit = num_in.toString().split(".");
  if (numSplit.length === 2) {
    if (num_in < 0) {
      pre = "-0.";
    }
    ret = parseFloat(pre + numSplit[1]);
  }

  return ret;
}