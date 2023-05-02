import {ICharacter} from "../types/_character.js";
import {INumeric} from "../types/_numeric.js";
import {ca} from "./ca.js";

export function na(left: number | string | ICharacter | INumeric, right: string | ICharacter) {
  return !ca(left, right);
}