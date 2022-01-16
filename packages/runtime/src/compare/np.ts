import {cp} from "./cp";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function np(left: number | string | ICharacter | INumeric, right: string | ICharacter) {
  return !cp(left, right);
}