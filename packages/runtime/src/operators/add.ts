import {Character, FieldSymbol, Float, Hex, Integer} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {String} from "../types/string";
import {parse} from "./_parse";

export function add(left: INumeric | ICharacter | string | number | Float | Integer | Hex | FieldSymbol,
                    right: INumeric | ICharacter | string | number  | Float | Integer | Hex | FieldSymbol): Integer | Float {

  if (left instanceof Integer && right instanceof Integer) {
    return new Integer().set(left.get() + right.get());
  } else if (typeof left === "number" && typeof right === "number"
      && Number.isInteger(left) && Number.isInteger(right)) {
    return new Integer().set(left + right);

  } else if (typeof left === "number" && Number.isInteger(left) && right instanceof Integer) {
    return new Integer().set(left + right.get());
  } else if (typeof right === "number" && Number.isInteger(right) && left instanceof Integer) {
    return new Integer().set(left.get() + right);

  } else if ((left instanceof String || left instanceof Character) && Number.isInteger(Number(left.get())) && right instanceof Integer) {
    return new Integer().set(Number.parseInt(left.get(), 10) + right.get());
  } else if ((right instanceof String || right instanceof Character) && Number.isInteger(Number(right)) && left instanceof Integer) {
    return new Integer().set(left.get() + Number.parseInt(right.get(), 10));
  }

  if (left instanceof FieldSymbol) {
    if (left.getPointer() === undefined) {
      throw new Error("GETWA_NOT_ASSIGNED");
    }
    return add(left.getPointer(), right);
  }

  if (right instanceof FieldSymbol) {
    if (right.getPointer() === undefined) {
      throw new Error("GETWA_NOT_ASSIGNED");
    }
    return add(left, right.getPointer());
  }

  const ret = new Float().set(parse(left) + parse(right));
  return ret;
}