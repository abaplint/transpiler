import {Integer, Integer8} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {parse} from "./_parse";

export function mod(left: INumeric | ICharacter | string | number, right: INumeric | ICharacter | string | number) {
  const l = parse(left);
  const r = parse(right);

  let val = ( ( l % r ) + r ) % r;
  if (l < 0 && r < 0 ) {
    val = val * -1;
  }

  if (left instanceof Integer8 || right instanceof Integer8) {
    return new Integer8().set(val);
  } else {
    return new Integer().set(val);
  }
}