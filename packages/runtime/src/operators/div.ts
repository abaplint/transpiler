import {throwError} from "../throw_error";
import {Float, Integer, Integer8} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {parse} from "./_parse";

export function div(left: INumeric | ICharacter | string | number, right: INumeric | ICharacter | string | number) {
  const l = parse(left);
  const r = parse(right);
  if (r === 0) {
    if (l === 0) {
      return new Integer().set(0);
    } else {
      throwError("CX_SY_ZERODIVIDE");
    }
  } else if (left instanceof Integer8 || right instanceof Integer8) {
    return new Integer8().set(Math.floor(l / r));
  } else {
    return new Float().set(Math.floor(l / r));
  }
}