
import {DecFloat34, Float} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function frac(input: {val: number | string | ICharacter | INumeric}) {
  let num_in: number | undefined = undefined;
  let ret = 0;
  let pre = "0.";
  if (typeof input.val === "number") {
    num_in = input.val;
  } else if (typeof input.val === "string") {
    num_in = parseFloat(input.val);
  } else if (input.val instanceof DecFloat34
      || input.val instanceof Float) {
    num_in = input.val.getRaw();
  } else {
    num_in = parseFloat(input.val.get().toString());
  }

  const numSplit = num_in.toString().split(".");
  if (numSplit.length === 2) {
    if (num_in < 0) {
      pre = "-0.";
    }
    ret = parseFloat(pre + numSplit[1]);
  }

  if (input.val instanceof DecFloat34) {
    return new DecFloat34().set(ret);
  } else if (input.val instanceof Float) {
    return new Float().set(ret);
  } else {
    return ret;
  }
}