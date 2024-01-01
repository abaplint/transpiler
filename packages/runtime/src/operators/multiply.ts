import {Character, Float, Integer, MAX_INTEGER, MIN_INTEGER} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {parse} from "./_parse";
import {String} from "../types/string";

export function multiply(left: INumeric | ICharacter | string | number, right: INumeric | ICharacter | string | number) {
  if (left instanceof Integer && right instanceof Integer) {
    const val = left.get() * right.get();
    if (val > MAX_INTEGER || val < MIN_INTEGER) {
      throw new Error("COMPUTE_INT_TIMES_OVERFLOW");
    }
    return new Integer().set(val);
  } else if (typeof left === "number" && typeof right === "number"
      && Number.isInteger(left) && Number.isInteger(right)) {
    const val = left * right;
    if (val > MAX_INTEGER || val < MIN_INTEGER) {
      throw new Error("COMPUTE_INT_TIMES_OVERFLOW");
    }
    return new Integer().set(val);
  } else if (typeof left === "number" && Number.isInteger(left) && right instanceof Integer) {
    const val = left * right.get();
    if (val > MAX_INTEGER || val < MIN_INTEGER) {
      throw new Error("COMPUTE_INT_TIMES_OVERFLOW");
    }
    return new Integer().set(val);
  } else if (typeof right === "number" && Number.isInteger(right) && left instanceof Integer) {
    const val = left.get() * right;
    if (val > MAX_INTEGER || val < MIN_INTEGER) {
      throw new Error("COMPUTE_INT_TIMES_OVERFLOW");
    }
    return new Integer().set(val);
  } else if ((left instanceof String || left instanceof Character) && Number.isInteger(Number(left.get())) && right instanceof Integer) {
    const val = Number.parseInt(left.get(), 10) * right.get();
    if (val > MAX_INTEGER || val < MIN_INTEGER) {
      throw new Error("COMPUTE_INT_TIMES_OVERFLOW");
    }
    return new Integer().set(val);
  } else if ((right instanceof String || right instanceof Character) && Number.isInteger(Number(right)) && left instanceof Integer) {
    const val = left.get() * Number.parseInt(right.get(), 10);
    if (val > MAX_INTEGER || val < MIN_INTEGER) {
      throw new Error("COMPUTE_INT_TIMES_OVERFLOW");
    }
    return new Integer().set(val);
  }

  return new Float().set(parse(left) * parse(right));
}