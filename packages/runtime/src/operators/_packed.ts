import {throwError} from "../throw_error";
import {FieldSymbol, Integer, Integer8, Packed} from "../types";

/** an operand next to a p: an i, an int8 or a whole number, as a BigInt; undefined for anything else */
function whole(operand: unknown): bigint | undefined {
  if (operand instanceof Integer) {
    return BigInt(operand.get());
  } else if (operand instanceof Integer8) {
    return operand.get();
  } else if (typeof operand === "number" && Number.isInteger(operand)) {
    return BigInt(operand);
  }
  return undefined;
}

/** "+", "-", "*" and MOD where one operand is p and the other is p, i or int8: the calculation
 * type is p and the result is exact, so it is computed on the scaled BigInt values and not
 * through a double. Returns undefined for any other pair of operands; the callers ask first
 * whether an operand is a p, so that other operands do not come here */
export function packedCalculation(operator: "+" | "-" | "*" | "MOD", left: unknown, right: unknown): Packed | undefined {
  if (left instanceof FieldSymbol) {
    left = left.getPointer();
  }
  if (right instanceof FieldSymbol) {
    right = right.getPointer();
  }
  const leftPacked = left instanceof Packed;
  const rightPacked = right instanceof Packed;
  if (leftPacked === false && rightPacked === false) {
    return undefined;
  }
  let lv = leftPacked ? (left as Packed).getScaled() : whole(left);
  let rv = rightPacked ? (right as Packed).getScaled() : whole(right);
  if (lv === undefined || rv === undefined) {
    return undefined;
  }
  const ld = leftPacked ? (left as Packed).getDecimals() : 0;
  const rd = rightPacked ? (right as Packed).getDecimals() : 0;
  if (operator === "*") {
    return Packed.calculated(lv * rv, ld + rd);
  }
  // the same number of decimals on both sides
  if (ld < rd) {
    lv = lv * 10n ** BigInt(rd - ld);
  } else if (rd < ld) {
    rv = rv * 10n ** BigInt(ld - rd);
  }
  const decimals = ld < rd ? rd : ld;
  if (operator === "+") {
    return Packed.calculated(lv + rv, decimals);
  } else if (operator === "-") {
    return Packed.calculated(lv - rv, decimals);
  }
  if (rv === 0n) {
    if (lv === 0n) {
      return Packed.calculated(0n, decimals);
    }
    throwError("CX_SY_ZERODIVIDE");
  }
  // the remainder is never negative and below |r|, whatever the signs
  const divisor = rv < 0n ? -rv : rv;
  const remainder = lv % divisor;
  return Packed.calculated(remainder < 0n ? remainder + divisor : remainder, decimals);
}
