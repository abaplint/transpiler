
import {FieldSymbol, Float, Integer8} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {exactNumericFunction} from "./_exact";
import {absDecimal} from "../types/_decimal";
import {int8Result} from "../types/integer8";

export function abs(input: {val: number | string | ICharacter | INumeric}) {
  let val: unknown = input.val;
  while (val instanceof FieldSymbol) {
    val = val.getPointer();
  }
  if (val instanceof Integer8) {
    // int8, exactly; abs( ) of -2^63 does not fit
    const v = val.get();
    return int8Result(v < 0n ? -v : v);
  }
  const exact = exactNumericFunction(input.val, absDecimal);
  if (exact !== undefined) {
    return exact;
  }
  let num_in: number | undefined = undefined;
  if (typeof input.val === "number") {
    num_in = input.val;
  } else if (typeof input.val === "string") {
    num_in = parseFloat(input.val);
  } else if (input.val instanceof Float) {
    num_in = input.val.getCalculationValue();
  } else {
    num_in = parseFloat(input.val.get().toString());
  }
  const ret = Math.abs(num_in);
  return ret;
}