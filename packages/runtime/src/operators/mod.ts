import {Integer, Integer8} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {parse} from "./_parse";

export function mod(left: INumeric | ICharacter | string | Integer8 | number, right: INumeric | Integer8 | ICharacter | string | number) {
  if (left instanceof Integer8 || right instanceof Integer8) {
    const l = left instanceof Integer8 ? left.get() : BigInt(parse(left));
    const r = right instanceof Integer8 ? right.get() : BigInt(parse(right));
    let val = ( ( l % r ) + r ) % r;
    if (val < 0) {
      val = val * -1n;
    }
    return new Integer8().set(val);
  }

  const l = parse(left);
  const r = parse(right);

  let val = ( ( l % r ) + r ) % r;

  if (val < 0) {
    val = val * -1;
  }

  // hmm, to handle field symbols?
  if (left instanceof Integer8 || right instanceof Integer8) {
    return new Integer8().set(val);
  } else {
    return new Integer().set(val);
  }
}