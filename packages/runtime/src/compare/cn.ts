import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {co} from "./co";

export function cn(left: number | string | ICharacter | INumeric, right: string | ICharacter): boolean {
  return co(left, right) === false;
}