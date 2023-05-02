import {Character, Float, Integer} from "../types/index.js";
import {ICharacter} from "../types/_character.js";
import {INumeric} from "../types/_numeric.js";
import {parse} from "./_parse.js";
import {String} from "../types/string.js";

export function multiply(left: INumeric | ICharacter | string | number, right: INumeric | ICharacter | string | number) {
  if (left instanceof Integer && right instanceof Integer) {
    return new Integer().set(left.get() * right.get());
  } else if (typeof left === "number" && typeof right === "number"
      && Number.isInteger(left) && Number.isInteger(right)) {
    return new Integer().set(left * right);

  } else if (typeof left === "number" && Number.isInteger(left) && right instanceof Integer) {
    return new Integer().set(left * right.get());
  } else if (typeof right === "number" && Number.isInteger(right) && left instanceof Integer) {
    return new Integer().set(left.get() * right);

  } else if ((left instanceof String || left instanceof Character) && Number.isInteger(Number(left.get())) && right instanceof Integer) {
    return new Integer().set(Number.parseInt(left.get(), 10) * right.get());
  } else if ((right instanceof String || right instanceof Character) && Number.isInteger(Number(right)) && left instanceof Integer) {
    return new Integer().set(left.get() * Number.parseInt(right.get(), 10));
  }

  return new Float().set(parse(left) * parse(right));
}