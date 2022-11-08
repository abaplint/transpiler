import {parse} from "../operators/_parse";
import {INumeric} from "../types/_numeric";
import {Float} from "../types";

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
  const values = [];
  values.push(parse(input.val1));
  values.push(parse(input.val2));
  if (input.val3) {
    values.push(parse(input.val3));
  }
  if (input.val4) {
    values.push(parse(input.val4));
  }
  if (input.val5) {
    values.push(parse(input.val5));
  }
  if (input.val6) {
    values.push(parse(input.val6));
  }
  if (input.val7) {
    values.push(parse(input.val7));
  }
  if (input.val8) {
    values.push(parse(input.val8));
  }
  if (input.val9) {
    values.push(parse(input.val9));
  }
  values.sort((a,b) => (b - a));
  return new Float().set(values[0]);
}