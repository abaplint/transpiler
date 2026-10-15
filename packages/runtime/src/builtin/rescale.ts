import {DecFloat34} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {round} from "./round";

type Input = INumeric | ICharacter | string | number;

/** rescale( val = arg {dec = n}|{prec = n} [mode = m] )
 *
 * Rounds like round( ) - the same arguments, the same rounding modes of
 * CL_ABAP_MATH, a decfloat34 result. What rescale( ) adds on a system is the
 * SCALE of the result: rescale( val = '1.2' dec = 3 ) is 1.200, with three
 * decimal places. DecFloat34 here keeps a number, not a number of decimal
 * places, so a scale increase changes nothing; the value, and every
 * calculation with it, is the same - only WRITE and string templates of such
 * a result show fewer trailing zeros. */
export function rescale(input: {val: Input, dec?: Input, prec?: Input, mode?: Input}): DecFloat34 {
  return round(input);
}
