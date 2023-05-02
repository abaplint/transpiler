import {ICharacter} from "../types/_character.js";
import {INumeric} from "../types/_numeric.js";
import {eq} from "./eq.js";

export function ne(left: number | string | ICharacter | INumeric, right: number | string | ICharacter | INumeric) {
  return !eq(left, right);
}