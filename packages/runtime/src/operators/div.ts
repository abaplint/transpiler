import {throwError} from "../throw_error";
import {Integer, Integer8} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {parse} from "./_parse";

export function div(left: INumeric | ICharacter | Integer8 | string | number, right: INumeric | Integer8 | ICharacter | string | number) {
  if (left instanceof Integer8 || right instanceof Integer8) {
    const l = left instanceof Integer8 ? left.get() : BigInt(parse(left));
    const r = right instanceof Integer8 ? right.get() : BigInt(parse(right));
    if (r === 0n) {
      if (l === 0n) {
        return new Integer8().set(0n);
      } else {
        throwError("CX_SY_ZERODIVIDE");
      }
    }
    // DIV leaves a remainder that is never negative, 0 <= l - r * div < |r|,
    // so a truncated quotient moves down for a positive divisor and up for
    // a negative one: 7 DIV -2 = -3, -7 DIV -2 = 4
    const remainder = l % r;
    let div = l / r;
    if (remainder < 0n) {
      div = r > 0n ? div - 1n : div + 1n;
    }
    return new Integer8().set(div);
  }

  const l = parse(left);
  const r = parse(right);
  if (r === 0) {
    if (l === 0) {
      return new Integer().set(0);
    } else {
      throwError("CX_SY_ZERODIVIDE");
    }
  } else {
    // floor only for a positive divisor; for a negative one the quotient
    // rounds up, so that the remainder a - b * ( a DIV b ) is not negative
    return new Integer().set(r > 0 ? Math.floor(l / r) : -Math.floor(l / -r));
  }
}