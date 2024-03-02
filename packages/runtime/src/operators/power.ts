import {Float, Integer8} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {parse} from "./_parse";

export function power(left: INumeric | ICharacter | Integer8 | string | number, right: INumeric | ICharacter | Integer8 | string | number) {
  if (left instanceof Integer8 || right instanceof Integer8) {
    const l = left instanceof Integer8 ? left.get() : BigInt(parse(left));
    const r = right instanceof Integer8 ? right.get() : BigInt(parse(right));
    return new Integer8().set(l ** r);
  }

  return new Float().set(Math.pow(parse(left), parse(right)));
}