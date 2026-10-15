import {DecFloat34, FieldSymbol, Packed} from "../types";
import {Decimal, decimalToNumber} from "../types/_decimal";
import {toDecimal} from "../operators/_decfloat";

/** floor, ceil, trunc, frac and abs of a decfloat34 or packed argument, calculated exactly: a
 * double holds neither 34 digits nor a TIMESTAMPL with seven decimals. A decfloat34 gives a
 * decfloat34, a packed number gives a number as before, only now without the digits lost on
 * the way. Undefined for any other argument. */
export function exactNumericFunction(val: unknown, calculate: (value: Decimal) => Decimal): DecFloat34 | number | undefined {
  let value = val;
  while (value instanceof FieldSymbol) {
    value = value.getPointer();
  }
  if (value instanceof DecFloat34) {
    return new DecFloat34().setDecimal(calculate(value.getDecimal()));
  } else if (value instanceof Packed) {
    return decimalToNumber(calculate(toDecimal(value)));
  }
  return undefined;
}
