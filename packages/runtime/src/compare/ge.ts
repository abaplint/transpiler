import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {gt, eq} from ".";

export function ge(left: number | string | ICharacter | INumeric, right: number | string | ICharacter | INumeric) {
  return gt(left, right) || eq(left, right);
}