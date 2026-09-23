import {throwError} from "../throw_error";
import {Float, Integer, Integer8} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {parse} from "./_parse";

export function mod(left: INumeric | ICharacter | string | Integer8 | number, right: INumeric | Integer8 | ICharacter | string | number) {
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

    // the remainder is never negative and below |r|, whatever the signs:
    // 7 MOD -3 = 1, -7 MOD -3 = 2
    const a = r < 0n ? -r : r;
    let val = l % a;
    if (val < 0n) {
      val = val + a;
    }
    return new Integer8().set(val);
  }

  const l = parse(left);
  const r = parse(right);
  if (r === 0) {
    if (l === 0) {
      return new Integer().set(0);
    } else {
      throwError("CX_SY_ZERODIVIDE");
    }
  }

  // a MOD r = a - r * ( a DIV r ), with the quotient rule of DIV:
  // 7 MOD -3 = 1, -7 MOD -3 = 2, 7.5 MOD -2 = 1.5. For f this is also what a
  // system computes in double precision, 0.5 MOD 0.1 = 0 there, where a
  // remainder by % gives 0.09999999999999998
  const q = r > 0 ? Math.floor(l / r) : -Math.floor(l / -r);
  let val = l - r * q;
  if (val === 0) {
    val = 0; // not -0
  }

  // the calculation type of the operands decides: with a float operand the
  // remainder is a float, 2.75 MOD 1 is 0.75 and not 1
  if (left instanceof Float || right instanceof Float) {
    return new Float().set(val);
  }
  return new Integer().set(val);
}