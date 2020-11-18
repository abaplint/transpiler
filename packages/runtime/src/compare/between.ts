import {INumeric} from "../types/_numeric";
import {ge} from "./ge";
import {le} from "./le";

export function between(left: number | INumeric, and1: number | INumeric, and2: number | INumeric): boolean {
  return ge(left, and1) && le(left, and2);
}