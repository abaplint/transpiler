/* eslint-disable radix */
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function ceil(input: number | string | ICharacter | INumeric) {
  let num_in: number | undefined = undefined;
  if (typeof input === "number") {
    num_in = input;
  } else if ( typeof input === "string") {
    num_in = parseFloat(input);
  } else {
    num_in = parseFloat(input.get().toString());
  }
  return Math.ceil(num_in);
}