import {Character, Float, Integer, Integer8} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {parse} from "./_parse";
import {String} from "../types/string";

export function multiply(left: INumeric | ICharacter | string | Integer8 | number,
                         right: INumeric | ICharacter | string | Integer8 | number) {
  if (left instanceof Integer8 || right instanceof Integer8) {
    const l = left instanceof Integer8 ? left.get() : BigInt(parse(left));
    const r = right instanceof Integer8 ? right.get() : BigInt(parse(right));
    return new Integer8().set(l * r);
  } else if (left instanceof Integer && right instanceof Integer) {
    const val = left.get() * right.get();
    return new Integer().set(val);
  } else if (typeof left === "number" && typeof right === "number"
      && Number.isInteger(left) && Number.isInteger(right)) {
    const val = left * right;
    return new Integer().set(val);
  } else if (typeof left === "number" && Number.isInteger(left) && right instanceof Integer) {
    const val = left * right.get();
    return new Integer().set(val);
  } else if (typeof right === "number" && Number.isInteger(right) && left instanceof Integer) {
    const val = left.get() * right;
    return new Integer().set(val);
  } else if ((left instanceof String || left instanceof Character) && Number.isInteger(Number(left.get())) && right instanceof Integer) {
    const val = Number.parseInt(left.get(), 10) * right.get();
    return new Integer().set(val);
  } else if ((right instanceof String || right instanceof Character) && Number.isInteger(Number(right)) && left instanceof Integer) {
    const val = left.get() * Number.parseInt(right.get(), 10);
    return new Integer().set(val);
  }

  return new Float().set(parse(left) * parse(right));
}