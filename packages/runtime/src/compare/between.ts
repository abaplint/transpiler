import {INumeric} from "../types/_numeric.js";
import {ge} from "./ge.js";
import {le} from "./le.js";

export function between(left: number | INumeric, and1: number | INumeric, and2: number | INumeric): boolean {
  return ge(left, and1) && le(left, and2);
}