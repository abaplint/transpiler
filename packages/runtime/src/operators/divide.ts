import {throwError} from "../throw_error";
import {Float, Integer} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {parse} from "./_parse";

// todo, this will only work when the target value is an integer?
export function divide(left: INumeric | ICharacter | string | number, right: INumeric | ICharacter | string | number) {
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