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
    // + 0 turns the -0 of 0 * -1 into 0
    const val = left.get() * right.get() + 0;
    return Integer.calculated(val);
  } else if (left instanceof Float && right instanceof Float) {
    // Two floats fall through the rest of the chain to exactly this, and
    // float arithmetic is the most common thing there is: the remaining type
    // tests all fail, and then parse() is called on each operand, which for a
    // Float is getRaw(). It sits after the integer branch on purpose, so that
    // integer arithmetic pays nothing for it.
    return new Float().set(left.getRaw() * right.getRaw()).setCalculated();
  } else if (typeof left === "number" && typeof right === "number"
      && Number.isInteger(left) && Number.isInteger(right)) {
    const val = left * right + 0;
    return Integer.calculated(val);
  } else if (typeof left === "number" && Number.isInteger(left) && right instanceof Integer) {
    const val = left * right.get() + 0;
    return Integer.calculated(val);
  } else if (typeof right === "number" && Number.isInteger(right) && left instanceof Integer) {
    const val = left.get() * right + 0;
    return Integer.calculated(val);
  } else if ((left instanceof String || left instanceof Character) && Number.isInteger(Number(left.get())) && right instanceof Integer) {
    const val = Number.parseInt(left.get(), 10) * right.get() + 0;
    return Integer.calculated(val).clearIntegerCalculationType();
  } else if ((right instanceof String || right instanceof Character) && Number.isInteger(Number(right)) && left instanceof Integer) {
    const val = left.get() * Number.parseInt(right.get(), 10) + 0;
    return Integer.calculated(val).clearIntegerCalculationType();
  }

  return new Float().set(parse(left) * parse(right)).setCalculated();
}