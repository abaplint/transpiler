import {INumeric} from "../types/_numeric";
import {Integer} from "../types/integer";

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
  values.push(get_nmax_val_input(input.val1));
  values.push(get_nmax_val_input(input.val2));
  if (input.val3) {
    values.push(get_nmax_val_input(input.val3));
  }
  if (input.val4) {
    values.push(get_nmax_val_input(input.val4));
  }
  if (input.val5) {
    values.push(get_nmax_val_input(input.val5));
  }
  if (input.val6) {
    values.push(get_nmax_val_input(input.val6));
  }
  if (input.val7) {
    values.push(get_nmax_val_input(input.val7));
  }
  if (input.val8) {
    values.push(get_nmax_val_input(input.val8));
  }
  if (input.val9) {
    values.push(get_nmax_val_input(input.val9));
  }
  values.sort((a,b) => (b - a));
  return new Integer().set(values[0]);
}

function get_nmax_val_input(val: number | INumeric) {
  if (typeof val === "number") {
    return val;
  } else {
    return val.get();
  }
}