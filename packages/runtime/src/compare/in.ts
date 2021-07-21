import {Table} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function compareIn(_left: number | string | ICharacter | INumeric, _right: Table): boolean {
  return true;
}