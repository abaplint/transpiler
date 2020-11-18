import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {cs} from "./cs";

export function ns(left: number | string | ICharacter | INumeric, right: string | ICharacter) {
  return !cs(left, right);
}