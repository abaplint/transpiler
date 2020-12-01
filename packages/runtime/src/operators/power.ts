import {Integer} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {parse} from "./_parse";

export function power(left: INumeric | ICharacter | string | number, right: INumeric | ICharacter | string | number) {
  return new Integer().set(Math.pow(parse(left), parse(right)));
}