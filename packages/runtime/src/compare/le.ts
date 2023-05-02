import {ICharacter} from "../types/_character.js";
import {INumeric} from "../types/_numeric.js";
import {lt, eq} from "./index.js";

export function le(left: number | string | ICharacter | INumeric, right: number | string | ICharacter | INumeric) {
  return lt(left, right) || eq(left, right);
}