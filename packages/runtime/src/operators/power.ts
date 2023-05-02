import {Float} from "../types/index.js";
import {ICharacter} from "../types/_character.js";
import {INumeric} from "../types/_numeric.js";
import {parse} from "./_parse.js";

export function power(left: INumeric | ICharacter | string | number, right: INumeric | ICharacter | string | number) {
  return new Float().set(Math.pow(parse(left), parse(right)));
}