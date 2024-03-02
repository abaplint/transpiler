import {Character, FieldSymbol, Float, Integer, Integer8} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {parse} from "./_parse";
import {String} from "../types/string";

export function minus(left: INumeric | ICharacter | string | Integer8 | number | Integer | Float | FieldSymbol,
                      right: INumeric | ICharacter | string | Integer8 | number | Integer | Float | FieldSymbol):
  Integer | Integer8 | Float {

  if (left instanceof FieldSymbol) {
    if (left.getPointer() === undefined) {
      throw new Error("GETWA_NOT_ASSIGNED");
    }
    return minus(left.getPointer(), right);
  }

  if (right instanceof FieldSymbol) {
    if (right.getPointer() === undefined) {
      throw new Error("GETWA_NOT_ASSIGNED");
    }
    return minus(left, right.getPointer());
  }

  if (left instanceof Integer8 || right instanceof Integer8) {
    const l = left instanceof Integer8 ? left.get() : BigInt(parse(left));
    const r = right instanceof Integer8 ? right.get() : BigInt(parse(right));
    return new Integer8().set(l - r);
  } else if (left instanceof Integer && right instanceof Integer) {
    return new Integer().set(left.get() - right.get());
  } else if (typeof left === "number" && typeof right === "number"
      && Number.isInteger(left) && Number.isInteger(right)) {
    return new Integer().set(left - right);

  } else if (typeof left === "number" && Number.isInteger(left) && right instanceof Integer) {
    return new Integer().set(left - right.get());
  } else if (typeof right === "number" && Number.isInteger(right) && left instanceof Integer) {
    return new Integer().set(left.get() - right);

  } else if ((left instanceof String || left instanceof Character) && Number.isInteger(Number(left.get())) && right instanceof Integer) {
    return new Integer().set(Number.parseInt(left.get(), 10) - right.get());
  } else if ((right instanceof String || right instanceof Character) && Number.isInteger(Number(right)) && left instanceof Integer) {
    return new Integer().set(left.get() - Number.parseInt(right.get(), 10));
  }

  return new Float().set(parse(left) - parse(right));
}