import {throwError} from "../throw_error.js";
import {Float} from "../types/index.js";
import {ICharacter} from "../types/_character.js";
import {INumeric} from "../types/_numeric.js";
import {parse} from "./_parse.js";

// todo, this will only work when the target value is an integer?
export function divide(left: INumeric | ICharacter | string | number, right: INumeric | ICharacter | string | number) {
  const r = parse(right);
  if (r === 0) {
    throwError("CX_SY_ZERODIVIDE");
  }
  const val = parse(left) / r;

  return new Float().set(val);
}