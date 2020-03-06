import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {lt, eq} from ".";

export function le(left: number | string | ICharacter | INumeric, right: number | string | ICharacter | INumeric) {
  return lt(left, right) || eq(left, right);
}