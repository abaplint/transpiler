import {throwError} from "../throw_error";
import {Float, Integer, Integer8} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {parse} from "./_parse";

// todo, this will only work when the target value is an integer?
export function divide(left: INumeric | ICharacter | Integer8 | string | number,
                       right: INumeric | ICharacter | Integer8 | string | number) {
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
    return new Integer8().set(l / r);
  }

  const r = parse(right);
  const l = parse(left);

  if (r === 0) {
    if (l === 0) {
      return new Integer().set(0);
    } else {
      throwError("CX_SY_ZERODIVIDE");
    }
  }
  const val = l / r;

  const ret = new Float().set(val).setCalculated();
  if (isIntegerOperand(left) && isIntegerOperand(right)) {
    ret.setIntegerCalculationType();
  }
  return ret;
}

function isIntegerOperand(val: INumeric | ICharacter | Integer8 | string | number): boolean {
  if (val instanceof Integer) {
    return val.isIntegerCalculationType();
  }
  return val instanceof Integer8;
}

/** "/" in calculation type int8, which the transpiler knows from an int8 target: the
 * operands are int8 and the quotient is rounded half away from zero, 7 / 2 = 4, -7 / 2 = -4 */
export function divideInt8(left: INumeric | ICharacter | Integer8 | string | number,
                           right: INumeric | ICharacter | Integer8 | string | number) {
  const l = left instanceof Integer8 ? left.get() : new Integer8().set(left as INumeric).get();
  const r = right instanceof Integer8 ? right.get() : new Integer8().set(right as INumeric).get();
  if (r === 0n) {
    if (l === 0n) {
      return new Integer8().set(0n);
    }
    throwError("CX_SY_ZERODIVIDE");
  }
  let q = l / r;
  const rem = l % r;
  if ((rem < 0n ? -rem : rem) * 2n >= (r < 0n ? -r : r)) {
    q = (l < 0n) === (r < 0n) ? q + 1n : q - 1n;
  }
  return new Integer8().set(q);
}
