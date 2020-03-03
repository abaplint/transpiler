import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {eq} from "./eq";

export function ne(left: number | string | ICharacter | INumeric, right: number | string | ICharacter | INumeric) {
  return !eq(left, right);
}