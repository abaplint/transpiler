import {divide} from "./divide";
import {mod} from "./mod";
import {div} from "./div";
import {multiply} from "./multiply";
import {minus} from "./minus";
import {add} from "./add";
import {Character, FieldSymbol, Integer, Integer8, Numc, Packed, String} from "../types";
import {throwError} from "../throw_error";

// Exact Number powers used only for safe integer scaling.
const NUMBER_POW10 = [1, 10, 100, 1000, 10000, 100000, 1000000, 10000000,
  100000000, 1000000000, 10000000000, 100000000000, 1000000000000,
  10000000000000, 100000000000000, 1000000000000000];

// Calculation type p, exact. Operate on integers scaled by 10^decimals:
// Number when all integer intermediates are safe, bigint otherwise. Using
// get(), an unscaled double, would lose precision for p values up to 31 digits.
//
// The typed packed store selects these operators. They return undefined for
// an operand that is not a packed, an i, an int8, a whole number
// or a decimal number in a text field. The caller then goes on as before.

// Common operands need no records or tuples. Text is scanned as integer digits,
// never converted through a floating decimal value or a string round trip.
function safeValue(value: unknown): number | undefined {
  if (value instanceof Packed) {
    return value.getSafeScaled();
  } else if (value instanceof Integer || value instanceof Integer8) {
    const n = Number(value.get());
    return Number.isSafeInteger(n) ? n : undefined;
  } else if (typeof value === "number") {
    return Number.isSafeInteger(value) ? value : undefined;
  } else if (value instanceof Character || value instanceof String || value instanceof Numc) {
    return safeValue(value.get());
  } else if (typeof value === "string") {
    let n = 0;
    let digits = 0;
    let dot = false;
    let i = value[0] === "-" || value[0] === "+" ? 1 : 0;
    for (; i < value.length; i++) {
      const c = value.charCodeAt(i);
      if (c === 46 && !dot) {
        dot = true;
      } else if (c >= 48 && c <= 57) {
        n = n * 10 + (c - 48);
        digits++;
        if (!Number.isSafeInteger(n)) {
          return undefined;
        }
      } else {
        return undefined;
      }
    }
    return digits === 0 ? undefined : value[0] === "-" ? -n : n;
  }
  return undefined;
}

function safeScale(value: unknown): number {
  if (value instanceof Packed) {
    return value.getDecimals();
  } else if (value instanceof Character || value instanceof String || value instanceof Numc) {
    return safeScale(value.get());
  } else if (typeof value === "string") {
    const dot = value.indexOf(".");
    return dot < 0 ? 0 : value.length - dot - 1;
  }
  return 0;
}

function safeTrimmed(value: number, scale: number): Packed {
  while (scale > 0 && value % 10 === 0) {
    value /= 10;
    scale--;
  }
  return Packed.fromScaled(value, scale);
}

type Operation = "add" | "minus" | "multiply" | "div" | "mod";

function fast(left: unknown, right: unknown, op: Operation): Packed | undefined {
  if (left instanceof FieldSymbol) {
    left = left.getPointer();
  }
  if (right instanceof FieldSymbol) {
    right = right.getPointer();
  }
  let a = safeValue(left);
  let b = safeValue(right);
  if (a === undefined || b === undefined) {
    return undefined;
  }
  const ls = safeScale(left);
  const rs = safeScale(right);
  if (op === "multiply") {
    const result = a * b;
    return Packed.fromScaled(Number.isSafeInteger(result) ? result : BigInt(a) * BigInt(b), ls + rs);
  }
  const scale = Math.max(ls, rs);
  if (scale - ls > 15 || scale - rs > 15) {
    return undefined;
  }
  if (scale !== ls) {
    a *= NUMBER_POW10[scale - ls];
  }
  if (scale !== rs) {
    b *= NUMBER_POW10[scale - rs];
  }
  if (!Number.isSafeInteger(a) || !Number.isSafeInteger(b)) {
    return undefined;
  }
  if (op === "add" || op === "minus") {
    const result = op === "add" ? a + b : a - b;
    const exact = Number.isSafeInteger(result) ? result : op === "add" ? BigInt(a) + BigInt(b) : BigInt(a) - BigInt(b);
    return Packed.fromScaled(exact, scale);
  }
  if (b === 0) {
    if (a === 0) {
      return Packed.fromScaled(0, op === "mod" ? scale : 0);
    }
    throwError("CX_SY_ZERODIVIDE");
  }
  const rem = a % b;
  if (op === "mod") {
    return safeTrimmed(rem < 0 ? rem + Math.abs(b) : rem, scale);
  }
  let result = (a - rem) / b;
  if (op === "div" && rem < 0) {
    result += b > 0 ? -1 : 1;
  }
  if (!Number.isSafeInteger(result)) {
    return undefined;
  }
  return Packed.fromScaled(result, 0);
}

type Scaled = {v: bigint, s: number};

// a number in a text field: digits with an optional sign and decimal point, no exponent
const DECIMAL = /^\s*([-+]?)(\d*)(?:\.(\d*))? *$/;

function scaledText(text: string): Scaled | undefined {
  const m = DECIMAL.exec(text);
  if (m === null || (m[2] === "" && (m[3] === undefined || m[3] === ""))) {
    return undefined;
  }
  const frac = m[3] ?? "";
  const v = BigInt((m[2] === "" ? "0" : m[2]) + frac);
  return {v: m[1] === "-" ? -v : v, s: frac.length};
}

function scaled(val: unknown): Scaled | undefined {
  if (val instanceof Packed) {
    return {v: val.getScaled(), s: val.getDecimals()};
  } else if (val instanceof Integer) {
    return {v: BigInt(val.get()), s: 0};
  } else if (val instanceof Integer8) {
    return {v: val.get(), s: 0};
  } else if (typeof val === "number" && Number.isInteger(val)) {
    return {v: BigInt(val), s: 0};
  } else if (val instanceof Character || val instanceof String || val instanceof Numc) {
    return scaledText(val.get());
  } else if (typeof val === "string") {
    return scaledText(val);
  }
  return undefined;
}

function operands(left: unknown, right: unknown): [Scaled, Scaled] | undefined {
  if (left instanceof FieldSymbol) {
    left = left.getPointer();
  }
  if (right instanceof FieldSymbol) {
    right = right.getPointer();
  }
  const l = scaled(left);
  const r = scaled(right);
  if (l === undefined || r === undefined) {
    return undefined;
  }
  return [l, r];
}

const POW10: bigint[] = [];
for (let i = 0; i <= 40; i++) {
  POW10.push(10n ** BigInt(i));
}

function align(x: Scaled, s: number): bigint {
  return x.s === s ? x.v : x.v * (POW10[s - x.s] ?? 10n ** BigInt(s - x.s));
}

function packedAdd(left: unknown, right: unknown): Packed | undefined {
  const quick = fast(left, right, "add");
  if (quick !== undefined) {
    return quick;
  }
  const ops = operands(left, right);
  if (ops === undefined) {
    return undefined;
  }
  const [l, r] = ops;
  const s = Math.max(l.s, r.s);
  return Packed.fromScaled(align(l, s) + align(r, s), s);
}

function packedMinus(left: unknown, right: unknown): Packed | undefined {
  const quick = fast(left, right, "minus");
  if (quick !== undefined) {
    return quick;
  }
  const ops = operands(left, right);
  if (ops === undefined) {
    return undefined;
  }
  const [l, r] = ops;
  const s = Math.max(l.s, r.s);
  return Packed.fromScaled(align(l, s) - align(r, s), s);
}

function packedMultiply(left: unknown, right: unknown): Packed | undefined {
  const quick = fast(left, right, "multiply");
  if (quick !== undefined) {
    return quick;
  }
  const ops = operands(left, right);
  if (ops === undefined) {
    return undefined;
  }
  const [l, r] = ops;
  return Packed.fromScaled(l.v * r.v, l.s + r.s);
}

// the quotient of DIV: the remainder a - b * q is never negative, 7 DIV -2 = -3, -7 DIV -2 = 4
function quotient(a: bigint, b: bigint): bigint {
  let q = a / b;
  if (a % b < 0n) {
    q = b > 0n ? q - 1n : q + 1n;
  }
  return q;
}

function packedDiv(left: unknown, right: unknown): Packed | undefined {
  const quick = fast(left, right, "div");
  if (quick !== undefined) {
    return quick;
  }
  const ops = operands(left, right);
  if (ops === undefined) {
    return undefined;
  }
  const [l, r] = ops;
  const s = Math.max(l.s, r.s);
  const a = align(l, s);
  const b = align(r, s);
  if (b === 0n) {
    if (a === 0n) {
      return Packed.fromScaled(0n, 0);
    }
    throwError("CX_SY_ZERODIVIDE");
  }
  return Packed.fromScaled(quotient(a, b), 0);
}

function packedMod(left: unknown, right: unknown): Packed | undefined {
  // The common large-value fallback also needs no operand records or tuple.
  if (left instanceof Packed && left.getSafeScaled() === undefined
      && ((typeof right === "number" && Number.isInteger(right)) || right instanceof Integer || right instanceof Integer8)) {
    const scale = left.getDecimals();
    let b = right instanceof Integer8 ? right.get() : BigInt(right instanceof Integer ? right.get() : right as number);
    if (scale !== 0) {
      b *= POW10[scale] ?? 10n ** BigInt(scale);
    }
    return bigintMod(left.getScaled(), b, scale);
  }
  const quick = fast(left, right, "mod");
  if (quick !== undefined) {
    return quick;
  }
  const ops = operands(left, right);
  if (ops === undefined) {
    return undefined;
  }
  const [l, r] = ops;
  const s = Math.max(l.s, r.s);
  const a = align(l, s);
  const b = align(r, s);
  return bigintMod(a, b, s);
}

function bigintMod(a: bigint, b: bigint, scale: number): Packed {
  if (b === 0n) {
    if (a === 0n) {
      return Packed.fromScaled(0n, scale);
    }
    throwError("CX_SY_ZERODIVIDE");
  }
  // Euclidean remainder directly; avoid bigint division and multiplication.
  const rem = a % b;
  return trimmed(rem < 0n ? rem + (b < 0n ? -b : b) : rem, scale);
}

function trimmed(value: bigint, scale: number): Packed {
  while (scale > 0 && value % 10n === 0n) {
    value /= 10n;
    scale--;
  }
  return Packed.fromScaled(value, scale);
}


export function addPacked(left: Parameters<typeof add>[0], right: Parameters<typeof add>[1]) {
  return packedAdd(left, right) ?? add(left, right);
}

export function minusPacked(left: Parameters<typeof minus>[0], right: Parameters<typeof minus>[1]) {
  return packedMinus(left, right) ?? minus(left, right);
}

export function multiplyPacked(left: Parameters<typeof multiply>[0], right: Parameters<typeof multiply>[1]) {
  return packedMultiply(left, right) ?? multiply(left, right);
}

export function divPacked(left: Parameters<typeof div>[0], right: Parameters<typeof div>[1]) {
  return packedDiv(left, right) ?? div(left, right);
}

export function modPacked(left: Parameters<typeof mod>[0], right: Parameters<typeof mod>[1]) {
  return packedMod(left, right) ?? mod(left, right);
}

// A quotient retains fourteen decimal places before the target rounds it.
// Scale the integer ratio directly so large integer parts remain exact.
export function dividePacked(left: Parameters<typeof divide>[0], right: Parameters<typeof divide>[1]) {
  const ops = operands(left, right);
  if (ops === undefined) {
    return divide(left, right);
  }
  const [l, r] = ops;
  if (r.v === 0n) {
    if (l.v === 0n) {
      return Packed.fromScaled(0, 14);
    }
    throwError("CX_SY_ZERODIVIDE");
  }
  const negative = (l.v < 0n) !== (r.v < 0n);
  let a = l.v < 0n ? -l.v : l.v;
  let b = r.v < 0n ? -r.v : r.v;
  const shift = 14 + r.s - l.s;
  if (shift >= 0) {
    a *= 10n ** BigInt(shift);
  } else {
    b *= 10n ** BigInt(-shift);
  }
  const q = a / b + (a % b * 2n >= b ? 1n : 0n);
  return Packed.fromScaled(negative ? -q : q, 14);
}
