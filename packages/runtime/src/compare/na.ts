import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {ca} from "./ca";

export function na(left: number | string | ICharacter | INumeric, right: string | ICharacter) {
  return !ca(left, right);
}