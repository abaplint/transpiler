import {Integer} from "../types/index.js";
import {ICharacter} from "../types/_character.js";
import {INumeric} from "../types/_numeric.js";
import {parse} from "./_parse.js";

export function div(left: INumeric | ICharacter | string | number, right: INumeric | ICharacter | string | number) {
  const l = parse(left);
  const r = parse(right);
  const ret = new Integer().set(Math.floor(l / r));
  return ret;
}