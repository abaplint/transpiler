import {Float} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {parse} from "./_parse";

// todo, this will only work when the target value is an integer?
export function divide(left: INumeric | ICharacter | string | number, right: INumeric | ICharacter | string | number) {
  const r = parse(right);
  if (r === 0) {
    // @ts-ignore
    if (abap.Classes["CX_SY_ZERODIVIDE"] !== undefined) {
      // @ts-ignore
      throw new abap.Classes["CX_SY_ZERODIVIDE"]();
    } else {
      throw "Global class CX_SY_ZERODIVIDE not found";
    }
  }
  const val = parse(left) / r;

  return new Float().set(val);
}