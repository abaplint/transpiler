import {Float, Integer} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {parse} from "./_parse";

export function minus(left: INumeric | ICharacter | string | number | Integer | Float,
                      right: INumeric | ICharacter | string | number | Integer | Float) {
  if (left instanceof Integer && right instanceof Integer) {
    return new Integer().set(left.get() - right.get());
  } else if (typeof left === "number" && typeof right === "number"
      && Number.isInteger(left) && Number.isInteger(right)) {
    return new Integer().set(left - right);
  } else if (typeof left === "number" && Number.isInteger(left) && right instanceof Integer) {
    return new Integer().set(left - right.get());
  } else if (typeof right === "number" && Number.isInteger(right) && left instanceof Integer) {
    return new Integer().set(left.get() - right);
  }
  return new Float().set(parse(left) - parse(right));
}