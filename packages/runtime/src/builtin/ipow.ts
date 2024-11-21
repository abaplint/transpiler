
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function ipow(input: {base: number | string | ICharacter | INumeric, exp: number | string | ICharacter | INumeric}) {
  let base: number | undefined = undefined;
  if (typeof input.base === "number") {
    base = input.base;
  } else if ( typeof input.base === "string") {
    base = parseFloat(input.base);
  } else {
    base = parseFloat(input.base.get().toString());
  }

  let exp: number | undefined = undefined;
  if (typeof input.exp === "number") {
    exp = input.exp;
  } else if ( typeof input.exp === "string") {
    exp = parseFloat(input.exp);
  } else {
    exp = parseFloat(input.exp.get().toString());
  }

  return Math.pow(base, exp).toFixed(0);
}