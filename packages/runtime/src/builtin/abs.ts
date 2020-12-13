/* eslint-disable radix */
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function abs(input: number | string | ICharacter | INumeric) {
  let num_in: number | undefined = undefined;
  if (typeof input === "number") {
    num_in = input;
  } else if ( typeof input === "string") {
    num_in = parseInt(input);
  } else {
    num_in = parseInt(input.get().toString());
  }
  return Math.abs(num_in);
}