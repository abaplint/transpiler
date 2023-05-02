import {ICharacter} from "../types/_character.js";
import {INumeric} from "../types/_numeric.js";
import {cs} from "./cs.js";

export function ns(left: number | string | ICharacter | INumeric, right: string | ICharacter) {
  return !cs(left, right);
}