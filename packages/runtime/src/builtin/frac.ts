
import {Float} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {exactNumericFunction} from "./_exact";
import {fracDecimal} from "../types/_decimal";

export function frac(input: {val: number | string | ICharacter | INumeric}) {
  const exact = exactNumericFunction(input.val, fracDecimal);
  if (exact !== undefined) {
    return exact;
  }
  let num_in: number | undefined = undefined;
  let ret = 0;
  let pre = "0.";
  if (typeof input.val === "number") {
    num_in = input.val;
  } else if (typeof input.val === "string") {
    num_in = parseFloat(input.val);
  } else if (input.val instanceof Float) {
    num_in = input.val.getCalculationValue();
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

  if (input.val instanceof Float) {
    return new Float().set(ret);
  } else {
    return ret;
  }
}