
import {Float} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {exactNumericFunction} from "./_exact";
import {ceilDecimal} from "../types/_decimal";

export function ceil(input: {val: number | string | ICharacter | INumeric}) {
  const exact = exactNumericFunction(input.val, ceilDecimal);
  if (exact !== undefined) {
    return exact;
  }
  let num_in: number | undefined = undefined;
  if (typeof input.val === "number") {
    num_in = input.val;
  } else if ( typeof input.val === "string") {
    num_in = parseFloat(input.val);
  } else if (input.val instanceof Float) {
    num_in = input.val.getCalculationValue();
  } else {
    num_in = parseFloat(input.val.get().toString());
  }
  return Math.ceil(num_in);
}