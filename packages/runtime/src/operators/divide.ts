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

  return new Float().set(val);
}