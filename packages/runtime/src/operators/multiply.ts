import {int8Result} from "../types/integer8";
import {Character, FieldSymbol, Float, Integer, Integer8, Packed} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {parse} from "./_parse";
import {asFloat, isFloatWithInteger8} from "./_float_integer8";
import {String} from "../types/string";
import {decFloat34Result, isDecFloat34Operand, toDecimal} from "./_decfloat";
import {multiplyDecimal} from "../types/_decimal";
import {packedCalculation} from "./_packed";

export function multiply(left: INumeric | ICharacter | string | Integer8 | number,
                         right: INumeric | ICharacter | string | Integer8 | number) {
  if (isDecFloat34Operand(left) || isDecFloat34Operand(right)) {
    return decFloat34Result(multiplyDecimal(toDecimal(left), toDecimal(right)));
  }
  if (left instanceof Integer && right instanceof Integer) {
    // first, so that integer arithmetic does not pay for the p check below
    return Integer.arithmeticResult(left.get() * right.get());
  }
  // a field symbol on a p is looked through here, the other paths below take a field symbol as they did
  if (left instanceof Packed || right instanceof Packed
      || (left instanceof FieldSymbol && left.getPointer() instanceof Packed)
      || (right instanceof FieldSymbol && right.getPointer() instanceof Packed)) {
    const packed = packedCalculation("*", left, right);
    if (packed !== undefined) {
      return packed;
    }
  }
  if (left instanceof Integer8 || right instanceof Integer8) {
    if (isFloatWithInteger8(left, right)) {
      return multiply(asFloat(left), asFloat(right));
    }
    const l = left instanceof Integer8 ? left.get() : BigInt(parse(left));
    const r = right instanceof Integer8 ? right.get() : BigInt(parse(right));
    return int8Result(l * r);
  } else if (left instanceof Float && right instanceof Float) {
    // Two floats fall through the rest of the chain to exactly this, and
    // float arithmetic is the most common thing there is: the remaining type
    // tests all fail, and then parse() is called on each operand, which for a
    // Float is getRaw(). It sits after the integer branch on purpose, so that
    // integer arithmetic pays nothing for it.
    return new Float().set(left.getRaw() * right.getRaw());
  } else if (typeof left === "number" && typeof right === "number"
      && Number.isInteger(left) && Number.isInteger(right)) {
    return Integer.arithmeticResult(left * right);
  } else if (typeof left === "number" && Number.isInteger(left) && right instanceof Integer) {
    return Integer.arithmeticResult(left * right.get());
  } else if (typeof right === "number" && Number.isInteger(right) && left instanceof Integer) {
    return Integer.arithmeticResult(left.get() * right);
  } else if ((left instanceof String || left instanceof Character) && Number.isInteger(Number(left.get())) && right instanceof Integer) {
    const val = Number.parseInt(left.get(), 10) * right.get();
    return Integer.arithmeticResult(val).clearIntegerCalculationType();
  } else if ((right instanceof String || right instanceof Character) && Number.isInteger(Number(right)) && left instanceof Integer) {
    const val = left.get() * Number.parseInt(right.get(), 10);
    return Integer.arithmeticResult(val).clearIntegerCalculationType();
  }

  return new Float().set(parse(left) * parse(right));
}