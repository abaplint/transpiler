import {Character, Date,Time,Hex, Float, Integer, DecFloat34, HexUInt8, Integer8} from "../types";
import {XString} from "../types/xstring";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {MAX_INTEGER, MIN_INTEGER} from "../types/integer";
import {throwError} from "../throw_error";

export function parse(val: INumeric | ICharacter | string | number | Float | Integer |Integer8): number {
  if (typeof val === "number") {
    return val;
  } else if (typeof val === "string") {
    const trimmed = val.trim();
    if (trimmed === "") {
      return 0;
    } else if (trimmed.includes(".")) {
      return parseFloat(trimmed);
    } else {
      return parseInt(trimmed, 10);
    }
  } else if (val instanceof Integer) {
    // optimize, as this is the most common case
    return val.get();
  } else if (val instanceof Float) {
    return val.getRaw();
  } else if (val instanceof Character) {
    // constants remember what they parse to, the value cannot change
    return val.getNumeric();
  } else if (val instanceof XString) {
    if (val.get() === "") {
      return 0;
    }
    return parseInt(val.get(), 16);
  } else if (val instanceof Hex || val instanceof HexUInt8) {
    let num = parseInt(val.get(), 16);
// handle two complement,
    if (val.getLength() >= 4) {
      const maxVal = Math.pow(2, val.get().length / 2 * 8);
      if (num > maxVal / 2 - 1) {
        num = num - maxVal;
      }
    }
    return num;
  } else if (val instanceof Time || val instanceof Date) {
    return val.getNumeric();
  } else if (val instanceof DecFloat34) {
    return val.getRaw();
  } else if (val instanceof Integer8) {
    const bigint = val.get();
    if (bigint > BigInt(Number.MAX_SAFE_INTEGER) || bigint < BigInt(Number.MIN_SAFE_INTEGER)) {
      throw new Error("int8 value too large for table expression index");
    }
    return Number(bigint);
  } else {
    return parse(val.get());
  }
}


/** an operand in a position of type i: an offset, a length, a table index, a loop bound.
 * An arithmetic result that does not fit into i raises CX_SY_ARITHMETIC_OVERFLOW there, as
 * it does when it is assigned to an i, instead of reading as an offset out of bounds or a
 * missing line */
export function parsePosition(val: INumeric | ICharacter | string | number | Float | Integer | Integer8): number {
  if (val instanceof Integer) {
    const value = val.get();
    if (value > MAX_INTEGER || value < MIN_INTEGER) {
      throwError("CX_SY_ARITHMETIC_OVERFLOW");
    }
    return value;
  } else if (val instanceof Float && val.isCalculated()) {
    const value = Math.round(val.getRaw());
    if (value > MAX_INTEGER || value < MIN_INTEGER) {
      throwError("CX_SY_ARITHMETIC_OVERFLOW");
    }
  }
  return parse(val);
}
