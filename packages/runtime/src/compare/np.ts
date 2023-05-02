import {cp} from "./cp.js";
import {ICharacter} from "../types/_character.js";
import {INumeric} from "../types/_numeric.js";

export function np(left: number | string | ICharacter | INumeric, right: string | ICharacter) {
  return !cp(left, right);
}