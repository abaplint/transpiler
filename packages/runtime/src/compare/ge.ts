import {ICharacter} from "../types/_character.js";
import {INumeric} from "../types/_numeric.js";
import {gt, eq} from "./index.js";

export function ge(left: number | string | ICharacter | INumeric, right: number | string | ICharacter | INumeric) {
  return gt(left, right) || eq(left, right);
}