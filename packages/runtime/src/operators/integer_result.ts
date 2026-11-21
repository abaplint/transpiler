import {throwError} from "../throw_error";
import {DecFloat34, Float, Integer, Integer8, Packed} from "../types";
import {MAX_INTEGER, MIN_INTEGER, roundHalfAwayFromZero} from "../types/integer";
import {roundToBigInt} from "../types/_decimal";

/** The result of an arithmetic expression or operation that is assigned to an i, or of an
 * operation in calculation type i. Outside the range of i it is CX_SY_ARITHMETIC_OVERFLOW,
 * a data object that does not fit is CX_SY_CONVERSION_OVERFLOW instead, see Integer.set().
 * The transpiler decides where the check goes, by the operands and the target: the runtime
 * evaluates an operation before it knows the target, and a p or int8 target takes
 * 2147483647 + 1 as it is. The value is returned as it is, the target converts it */
export function integerResult<T>(value: T): T {
  let rounded: number;
  if (value instanceof Integer) {
    rounded = value.get();
  } else if (typeof value === "number") {
    rounded = roundHalfAwayFromZero(value);
  } else if (value instanceof Integer8) {
    rounded = Number(value.get());
  } else if (value instanceof Packed) {
    rounded = Number(value.toFixed(0));
  } else if (value instanceof DecFloat34) {
    rounded = Number(roundToBigInt(value.getDecimal()));
  } else if (value instanceof Float) {
    rounded = roundHalfAwayFromZero(value.getRaw());
  } else {
    return value;
  }
  if (rounded > MAX_INTEGER || rounded < MIN_INTEGER) {
    throwError("CX_SY_ARITHMETIC_OVERFLOW");
  }
  return value;
}
