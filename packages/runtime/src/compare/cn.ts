import {ICharacter} from "../types/_character.js";
import {INumeric} from "../types/_numeric.js";
import {co} from "./co.js";

export function cn(left: number | string | ICharacter | INumeric, right: string | ICharacter): boolean {
  return co(left, right) === false;
}