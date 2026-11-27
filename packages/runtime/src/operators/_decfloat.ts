import {ABAPObject, Character, DecFloat34, FieldSymbol, Float, HashedTable, Hex, HexUInt8, Integer, Integer8, Packed, Structure, Table} from "../types";
import {String} from "../types/string";
import {XString} from "../types/xstring";
import {compareDecimal, Decimal, decimalFromBigInt, decimalFromNumber, decimalToFixed, isZero, parseDecimal} from "../types/_decimal";
import {throwError} from "../throw_error";
import {parse} from "./_parse";

/** An arithmetic expression has calculation type decfloat34 when one of its operands is decfloat34 */
export function isDecFloat34Operand(value: unknown): boolean {
  if (value instanceof FieldSymbol) {
    return value.getPointer() instanceof DecFloat34;
  }
  return value instanceof DecFloat34;
}

function unwrap(value: unknown): unknown {
  return value instanceof FieldSymbol ? value.getPointer() : value;
}

/** An arithmetic expression has calculation type p when an operand is packed and none is
 * f or decfloat34 (decfloat34 is tested first). It is computed exactly: through a double,
 * a 21-digit p loses its last digits, 20261014103400.1234567 + '0.0000001' gave .1235968 */
export function isPackedCalculation(left: unknown, right: unknown): boolean {
  const l = unwrap(left);
  const r = unwrap(right);
  return (l instanceof Packed || r instanceof Packed) && !(l instanceof Float) && !(r instanceof Float);
}

/** The decimals of the packed operands, the most of them */
export function packedDecimals(left: unknown, right: unknown): number {
  const l = unwrap(left);
  const r = unwrap(right);
  return Math.max(l instanceof Packed ? l.getDecimals() : 0, r instanceof Packed ? r.getDecimals() : 0);
}

/** An intermediate result in calculation type p, with at least the given decimals and all the
 * digits of the value: no length, so it never overflows, and the target's set( ) checks its
 * own length and rescales the value exactly */
export function packedResult(value: Decimal, decimals: number): Packed {
  const scale = Math.max(decimals, value.scale);
  return new Packed({decimals: scale}).set(decimalToFixed(value, scale));
}

/** An operand as an exact decimal; text that is not a number raises CX_SY_CONVERSION_NO_NUMBER */
export function toDecimal(value: any): Decimal {
  if (value instanceof FieldSymbol) {
    if (value.getPointer() === undefined) {
      throw new Error("GETWA_NOT_ASSIGNED");
    }
    return toDecimal(value.getPointer());
  } else if (value instanceof DecFloat34) {
    return value.getDecimal();
  } else if (typeof value === "number") {
    return decimalFromNumber(value);
  } else if (typeof value === "bigint") {
    return decimalFromBigInt(value);
  } else if (typeof value === "string" || value instanceof Character || value instanceof String) {
    const parsed = parseDecimal(typeof value === "string" ? value : value.get());
    if (parsed === undefined) {
      throwError("CX_SY_CONVERSION_NO_NUMBER");
    }
    return parsed!;
  } else if (value instanceof Integer) {
    return decimalFromBigInt(BigInt(value.get()));
  } else if (value instanceof Integer8) {
    return decimalFromBigInt(value.get());
  } else if (value instanceof Packed) {
    return parseDecimal(value.toFixed(value.getDecimals()))!;
  } else if (value instanceof Float) {
    return decimalFromNumber(value.getRaw());
  }
  return decimalFromNumber(parse(value));
}

/** Like toDecimal, but undefined for an operand that is not compared as a number */
export function tryDecimal(value: any): Decimal | undefined {
  if (value instanceof FieldSymbol) {
    return value.getPointer() === undefined ? undefined : tryDecimal(value.getPointer());
  } else if (value === undefined
      || value instanceof Structure
      || value instanceof Table
      || value instanceof HashedTable
      || value instanceof ABAPObject
      || value instanceof Hex
      || value instanceof HexUInt8
      || value instanceof XString) {
    return undefined;
  } else if (typeof value === "string" || value instanceof Character || value instanceof String) {
    return parseDecimal(typeof value === "string" ? value : value.get());
  }
  return toDecimal(value);
}

export function decFloat34Result(value: Decimal): DecFloat34 {
  return new DecFloat34().setDecimal(value);
}

export function checkDivisor(left: Decimal, right: Decimal): boolean {
  if (isZero(right)) {
    if (isZero(left)) {
      return false;
    }
    throwError("CX_SY_ZERODIVIDE");
  }
  return true;
}

/** Compares exactly when one operand is decfloat34; undefined when the other one is not a number */
export function compareDecFloat34(left: unknown, right: unknown): number | undefined {
  const l = tryDecimal(left);
  const r = l === undefined ? undefined : tryDecimal(right);
  return r === undefined ? undefined : compareDecimal(l!, r);
}
