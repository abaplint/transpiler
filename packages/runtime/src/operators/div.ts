import {Integer} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {parse} from "./_parse";

export function div(left: INumeric | ICharacter | string | number, right: INumeric | ICharacter | string | number) {
  return new Integer().set(Math.floor(parse(left) / parse(right)));
}