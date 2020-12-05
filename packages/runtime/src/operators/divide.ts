import {Integer} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {parse} from "./_parse";

// todo, this will only work when the target value is an integer?
export function divide(left: INumeric | ICharacter | string | number, right: INumeric | ICharacter | string | number) {
  const val = parse(left) / parse(right);
  return new Integer().set(Math.round(val));
}