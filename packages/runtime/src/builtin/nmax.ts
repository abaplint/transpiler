import {extremum} from "./_extremum";
import {INumeric} from "../types/_numeric";

export interface INmaxInput {
  val1: number | INumeric,
  val2: number | INumeric,
  val3?: number | INumeric,
  val4?: number | INumeric,
  val5?: number | INumeric,
  val6?: number | INumeric,
  val7?: number | INumeric,
  val8?: number | INumeric,
  val9?: number | INumeric,
}

export function nmax(input: INmaxInput) {
  return extremum([input.val1, input.val2, input.val3, input.val4, input.val5,
    input.val6, input.val7, input.val8, input.val9], (candidate, best) => candidate > best);
}
