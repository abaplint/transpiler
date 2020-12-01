import {Integer} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {parse} from "./_parse";

export function mod(left: INumeric | ICharacter | string | number, right: INumeric | ICharacter | string | number) {
  return new Integer().set(parse(left) % parse(right));
}