import {Integer, Integer8} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {parse} from "./_parse";

export function mod(left: INumeric | ICharacter | string | number, right: INumeric | ICharacter | string | number) {
  const l = parse(left);
  const r = parse(right);
  if (left instanceof Integer8 || right instanceof Integer8) {
    return new Integer8().set(( ( l % r ) + r ) % r);
  } else {
    return new Integer().set(( ( l % r ) + r ) % r);
  }
}