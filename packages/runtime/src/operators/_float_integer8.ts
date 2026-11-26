import {Float, Integer8} from "../types";
import {INumeric} from "../types/_numeric";

/** f takes precedence over int8 in the calculation type: an operation with an f operand and an
 *  int8 operand is calculated in f, so the operators call themselves again with asFloat( ) of both */
export function isFloatWithInteger8(left: unknown, right: unknown): boolean {
  return (left instanceof Float && right instanceof Integer8)
    || (left instanceof Integer8 && right instanceof Float);
}

/** an operand of isFloatWithInteger8 as an f, an int8 beyond 2^53 is rounded; typed as the
 *  operators take it, they handle a Float operand */
export function asFloat(val: unknown): INumeric {
  const ret = val instanceof Integer8 ? new Float().set(Number(val.get())) : val as Float;
  return ret as unknown as INumeric;
}
