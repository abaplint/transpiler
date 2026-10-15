/** An exact decimal number, digits · 10^-scale, with a scale that is never negative.
 * decfloat34 keeps its value like this, so a calculation gives the digits ABAP gives,
 * not the nearest binary double. */
export interface Decimal {
  digits: bigint;
  scale: number;
}

/** decfloat34 has 34 significant digits */
export const DECFLOAT34_PRECISION = 34;

const NUMBER = /^([+-]?)(\d*)(?:[.,](\d*))?(?:[eE]([+-]?\d+))?([+-]?)$/;

function pow10(exponent: number): bigint {
  return 10n ** BigInt(exponent);
}

function abs(value: bigint): bigint {
  return value < 0n ? -value : value;
}

function digitCount(value: bigint): number {
  return value === 0n ? 1 : abs(value).toString().length;
}

/** Removes trailing zeros of the fraction, 1.50 is 1.5 */
function normalize(value: Decimal): Decimal {
  let digits = value.digits;
  let scale = value.scale;
  while (scale > 0 && digits % 10n === 0n) {
    digits = digits / 10n;
    scale--;
  }
  if (scale < 0) {
    digits = digits * pow10(-scale);
    scale = 0;
  }
  return {digits, scale};
}

/** Rounds to the given number of significant digits, half away from zero */
export function roundSignificant(value: Decimal, precision = DECFLOAT34_PRECISION): Decimal {
  const drop = digitCount(value.digits) - precision;
  if (drop <= 0) {
    return normalize(value);
  }
  const divisor = pow10(drop);
  const magnitude = abs(value.digits);
  let kept = magnitude / divisor;
  if ((magnitude % divisor) * 2n >= divisor) {
    kept = kept + 1n;
  }
  return normalize({digits: value.digits < 0n ? -kept : kept, scale: value.scale - drop});
}

/** Reads a number written as text: blanks around it, a sign in front or behind, a period or
 * a comma as decimal separator and an exponent are allowed. Undefined if it is not a number. */
export function parseDecimal(text: string): Decimal | undefined {
  const trimmed = text.trim();
  if (trimmed === "") {
    return {digits: 0n, scale: 0};
  }
  const match = NUMBER.exec(trimmed);
  if (match === null) {
    return undefined;
  }
  const [, leading, integer, fraction = "", exponent = "0", trailing] = match;
  if (integer === "" && fraction === "") {
    return undefined;
  } else if (leading !== "" && trailing !== "") {
    return undefined;
  }
  const negative = leading === "-" || trailing === "-";
  const digits = BigInt((integer || "0") + fraction);
  return roundSignificant({digits: negative ? -digits : digits, scale: fraction.length - parseInt(exponent, 10)});
}

/** A JavaScript number as the shortest decimal that reads back as the same number, 0.1 is 0.1 */
export function decimalFromNumber(value: number): Decimal {
  if (Number.isFinite(value) === false) {
    throw new Error("decfloat34, not a finite number: " + value);
  }
  return parseDecimal(String(value))!;
}

export function decimalFromBigInt(value: bigint): Decimal {
  return roundSignificant({digits: value, scale: 0});
}

/** Plain notation without exponent, trailing zeros of the fraction removed */
export function decimalToString(value: Decimal, separator = "."): string {
  const negative = value.digits < 0n;
  let text = abs(value.digits).toString();
  if (value.scale > 0) {
    text = text.padStart(value.scale + 1, "0");
    text = text.substring(0, text.length - value.scale) + separator + text.substring(text.length - value.scale);
  }
  return (negative ? "-" : "") + text;
}

export function decimalToNumber(value: Decimal): number {
  return Number(decimalToString(value));
}

function align(left: Decimal, right: Decimal): [bigint, bigint, number] {
  const scale = Math.max(left.scale, right.scale);
  return [left.digits * pow10(scale - left.scale), right.digits * pow10(scale - right.scale), scale];
}

export function compareDecimal(left: Decimal, right: Decimal): number {
  const [l, r] = align(left, right);
  return l === r ? 0 : l > r ? 1 : -1;
}

export function isZero(value: Decimal): boolean {
  return value.digits === 0n;
}

export function addDecimal(left: Decimal, right: Decimal): Decimal {
  const [l, r, scale] = align(left, right);
  return roundSignificant({digits: l + r, scale});
}

export function subtractDecimal(left: Decimal, right: Decimal): Decimal {
  const [l, r, scale] = align(left, right);
  return roundSignificant({digits: l - r, scale});
}

export function multiplyDecimal(left: Decimal, right: Decimal): Decimal {
  return roundSignificant({digits: left.digits * right.digits, scale: left.scale + right.scale});
}

/** The quotient to 34 significant digits. The digits behind are cut off first, which does not
 * change the first of them, so rounding half away from zero afterwards is exact. The divisor
 * must not be zero. */
export function divideDecimal(left: Decimal, right: Decimal): Decimal {
  const extra = Math.max(0, DECFLOAT34_PRECISION + 2 + digitCount(right.digits) - digitCount(left.digits));
  const quotient = (left.digits * pow10(extra)) / right.digits;
  return roundSignificant({digits: quotient, scale: left.scale + extra - right.scale});
}

/** The integer part, cut off towards zero */
export function truncDecimal(value: Decimal): Decimal {
  return {digits: value.digits / pow10(value.scale), scale: 0};
}

export function floorDecimal(value: Decimal): Decimal {
  const whole = truncDecimal(value);
  return value.digits < 0n && compareDecimal(whole, value) !== 0 ? {digits: whole.digits - 1n, scale: 0} : whole;
}

export function ceilDecimal(value: Decimal): Decimal {
  const whole = truncDecimal(value);
  return value.digits > 0n && compareDecimal(whole, value) !== 0 ? {digits: whole.digits + 1n, scale: 0} : whole;
}

/** The fraction, with the sign of the value */
export function fracDecimal(value: Decimal): Decimal {
  return subtractDecimal(value, truncDecimal(value));
}

export function absDecimal(value: Decimal): Decimal {
  return {digits: abs(value.digits), scale: value.scale};
}

/** Rounded to an integer, half away from zero */
export function roundToBigInt(value: Decimal): bigint {
  const divisor = pow10(value.scale);
  const magnitude = abs(value.digits);
  let whole = magnitude / divisor;
  if ((magnitude % divisor) * 2n >= divisor && value.scale > 0) {
    whole = whole + 1n;
  }
  return value.digits < 0n ? -whole : whole;
}

/** Rounded half away from zero to the given number of decimal places, in plain notation:
 * 2.675 gives 2.68, where toFixed( ) on the nearest double gives 2.67 */
export function decimalToFixed(value: Decimal, decimals: number): string {
  let rounded: Decimal;
  if (value.scale <= decimals) {
    rounded = {digits: value.digits * pow10(decimals - value.scale), scale: decimals};
  } else {
    const shifted = roundToBigInt({digits: value.digits, scale: value.scale - decimals});
    rounded = {digits: shifted, scale: decimals};
  }
  return decimalToString(rounded);
}
