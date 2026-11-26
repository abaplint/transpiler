import {throwError} from "../throw_error";
import {FieldSymbol} from "../types/field_symbol";
import {Integer, MAX_INTEGER, MIN_INTEGER} from "../types/integer";

type Operand = {get(): number} | number;

function value(operand: Operand): number {
  if (typeof operand === "number") {
    return operand;
  } else if (operand instanceof FieldSymbol && operand.getPointer() === undefined) {
    throw new Error("GETWA_NOT_ASSIGNED");
  }
  return operand.get();
}

function result(val: number): Integer {
  if (val > MAX_INTEGER || val < MIN_INTEGER) {
    throwError("CX_SY_ARITHMETIC_OVERFLOW");
  }
  return new Integer().set(val);
}

/** "+", "-" and "*" in calculation type i, which the transpiler knows from an i target with
 * operands of type i: every operation is checked, so an intermediate result outside the range
 * of i raises CX_SY_ARITHMETIC_OVERFLOW too, max + 1 - 1 does not come back as max */
export function addInt(left: Operand, right: Operand): Integer {
  return result(value(left) + value(right));
}

export function minusInt(left: Operand, right: Operand): Integer {
  return result(value(left) - value(right));
}

export function multiplyInt(left: Operand, right: Operand): Integer {
  return result(value(left) * value(right));
}
