import {parse} from "../operators/_parse";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

/** Reads an offset, length or occurrence argument as a number.
 *
 * parse() rather than get(): INumeric.get() promises a number, but Float and DecFloat34
 * hand out their ABAP display form instead, ie. "1,2000000000000000E+01". Reading that
 * back as a position keeps only the leading digit of the mantissa, so a length of 12
 * silently becomes 1. nmin( ) and nmax( ) return a Float, and the syntax check lets them
 * through where it rejects a declared TYPE f, so they are the way a float gets here.
 *
 * Rounded because ABAP converts the argument to type i, the same as assigning to one. */
export function position(input: INumeric | ICharacter | string | number | undefined): number | undefined {
  if (input === undefined) {
    return undefined;
  }
  return Math.round(parse(input));
}
