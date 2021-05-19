import {Integer} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {parse} from "./_parse";

export function mod(left: INumeric | ICharacter | string | number, right: INumeric | ICharacter | string | number) {
  const ret = new Integer().set(parse(left) % parse(right));
  return ret;
}