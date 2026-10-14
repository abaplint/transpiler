import {Character, DecFloat34, FieldSymbol, Float, Integer, Integer8, Packed, String, UTCLong} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {throwError} from "../throw_error";

// A utclong value is handled as a count of 100 ns ticks since 0001-01-01 00:00:00.0000000,
// so that adding seconds and taking differences is exact for the whole range of the type.

export type UTCLongInput = UTCLong | FieldSymbol | ICharacter | string;
export type NumberInput = INumeric | ICharacter | FieldSymbol | Integer8 | number | string;

const TICKS_PER_SECOND = 10000000n;
const TICKS_PER_DAY = 86400n * TICKS_PER_SECOND;
const FRACTION_DIGITS = 7;

const format = /^(\d{4})-(\d{2})-(\d{2})[ T](\d{2}):(\d{2}):(\d{2})(?:[.,](\d{1,7}))?$/;

// days from 1970-01-01 to the given date of the proleptic Gregorian calendar
function daysFromCivil(year: number, month: number, day: number): number {
  const y = month <= 2 ? year - 1 : year;
  const era = Math.floor(y / 400);
  const yearOfEra = y - era * 400;
  const dayOfYear = Math.floor((153 * (month > 2 ? month - 3 : month + 9) + 2) / 5) + day - 1;
  const dayOfEra = yearOfEra * 365 + Math.floor(yearOfEra / 4) - Math.floor(yearOfEra / 100) + dayOfYear;
  return era * 146097 + dayOfEra - 719468;
}

function civilFromDays(days: number): {year: number, month: number, day: number} {
  const z = days + 719468;
  const era = Math.floor(z / 146097);
  const dayOfEra = z - era * 146097;
  const yearOfEra = Math.floor((dayOfEra - Math.floor(dayOfEra / 1460) + Math.floor(dayOfEra / 36524)
    - Math.floor(dayOfEra / 146096)) / 365);
  const dayOfYear = dayOfEra - (365 * yearOfEra + Math.floor(yearOfEra / 4) - Math.floor(yearOfEra / 100));
  const mp = Math.floor((5 * dayOfYear + 2) / 153);
  const day = dayOfYear - Math.floor((153 * mp + 2) / 5) + 1;
  const month = mp < 10 ? mp + 3 : mp - 9;
  return {year: yearOfEra + era * 400 + (month <= 2 ? 1 : 0), month, day};
}

const FIRST_DAY = daysFromCivil(1, 1, 1);

function daysInMonth(year: number, month: number): number {
  return civilFromDays(daysFromCivil(month === 12 ? year + 1 : year, month === 12 ? 1 : month + 1, 1) - 1).day;
}

function textOf(input: UTCLongInput): string {
  let value: UTCLongInput | undefined = input;
  while (value instanceof FieldSymbol) {
    value = value.getPointer() as UTCLongInput | undefined;
    if (value === undefined) {
      throw new Error("GETWA_NOT_ASSIGNED");
    }
  }
  return (typeof value === "string" ? value : value.get()).trim();
}

/** Ticks of a time stamp; an initial value counts as the lowest time stamp. */
export function utclongToTicks(input: UTCLongInput): bigint {
  const text = textOf(input);
  if (text === "" || /^[0\-: T.,]*$/.test(text)) {
    return 0n;
  }

  const parts = format.exec(text);
  if (parts === null) {
    throwError("CX_SY_CONVERSION_NO_DATE_TIME");
  }
  const [year, month, day, hour, minute, second] = parts.slice(1, 7).map(p => parseInt(p, 10));
  if (year < 1 || month < 1 || month > 12 || day < 1 || day > daysInMonth(year, month)
      || hour > 23 || minute > 59 || second > 59) {
    throwError("CX_SY_CONVERSION_NO_DATE_TIME");
  }

  const days = BigInt(daysFromCivil(year, month, day) - FIRST_DAY);
  const seconds = BigInt(hour * 3600 + minute * 60 + second);
  return days * TICKS_PER_DAY + seconds * TICKS_PER_SECOND + BigInt((parts[7] ?? "").padEnd(FRACTION_DIGITS, "0"));
}

const MAX_TICKS = utclongToTicks("9999-12-31 23:59:59.9999999");

export function isInRange(ticks: bigint): boolean {
  return ticks >= 0n && ticks <= MAX_TICKS;
}

export function ticksToUTCLong(ticks: bigint): UTCLong {
  const days = Number(ticks / TICKS_PER_DAY);
  const rest = ticks % TICKS_PER_DAY;
  const date = civilFromDays(days + FIRST_DAY);
  const seconds = Number(rest / TICKS_PER_SECOND);
  const fraction = (rest % TICKS_PER_SECOND).toString().padStart(FRACTION_DIGITS, "0");
  const pad = (n: number, length = 2) => n.toString().padStart(length, "0");
  return new UTCLong().set(`${pad(date.year, 4)}-${pad(date.month)}-${pad(date.day)} `
    + `${pad(Math.floor(seconds / 3600))}:${pad(Math.floor(seconds / 60) % 60)}:${pad(seconds % 60)}.${fraction}`);
}

export function currentTicks(): bigint {
  const sinceEpoch = BigInt(Date.now()) * 10000n;
  return BigInt(daysFromCivil(1970, 1, 1) - FIRST_DAY) * TICKS_PER_DAY + sinceEpoch;
}

/** The number as a decimal text without exponent, as exact as the source type keeps it. */
function decimalText(input: NumberInput): string {
  let value: NumberInput | undefined = input;
  while (value instanceof FieldSymbol) {
    value = value.getPointer() as NumberInput | undefined;
    if (value === undefined) {
      throw new Error("GETWA_NOT_ASSIGNED");
    }
  }

  if (typeof value === "number") {
    return Number.isInteger(value) ? BigInt(value).toString() : value.toFixed(FRACTION_DIGITS + 1);
  } else if (typeof value === "string") {
    return value.trim();
  } else if (value instanceof Packed) {
    return value.toFixed(value.getDecimals());
  } else if (value instanceof Integer8) {
    return value.get().toString();
  } else if (value instanceof Integer) {
    return value.get().toString();
  } else if (value instanceof Float || value instanceof DecFloat34) {
    return decimalText(value.getRaw());
  } else if (value instanceof Character || value instanceof String) {
    return value.get().trim();
  }
  return decimalText((value as INumeric).get());
}

/** Rounds a decimal text half away from zero to the given number of decimal places, as a scaled bigint. */
function scaled(text: string, decimals: number): bigint {
  const match = /^([+-]?)(\d*)(?:[.,](\d*))?$/.exec(text.replace(/^([+-]?)\s+/, "$1"));
  if (match === null) {
    throwError("CX_SY_CONVERSION_NO_NUMBER");
  }
  const fraction = match[3] ?? "";
  let magnitude = BigInt((match[2] || "0") + fraction.substring(0, decimals).padEnd(decimals, "0"));
  if (fraction.length > decimals && fraction[decimals] >= "5") {
    magnitude += 1n;
  }
  return match[1] === "-" ? -magnitude : magnitude;
}

/** A count of whole units (days, hours, minutes), converted like a number to i or int8. */
export function wholeUnits(input: NumberInput | undefined): bigint {
  return input === undefined ? 0n : scaled(decimalText(input), 0);
}

/** Seconds as ticks, rounded to seven decimal places. */
export function secondsToTicks(input: NumberInput | undefined): bigint {
  return input === undefined ? 0n : scaled(decimalText(input), FRACTION_DIGITS);
}

/** A tick count as seconds, exact to seven decimal places. */
export function ticksToSeconds(ticks: bigint): DecFloat34 {
  const negative = ticks < 0n;
  const magnitude = negative ? -ticks : ticks;
  const fraction = (magnitude % TICKS_PER_SECOND).toString().padStart(FRACTION_DIGITS, "0");
  return new DecFloat34().set((negative ? "-" : "") + (magnitude / TICKS_PER_SECOND).toString() + "." + fraction);
}

export const TICKS = {second: TICKS_PER_SECOND, minute: 60n * TICKS_PER_SECOND, hour: 3600n * TICKS_PER_SECOND, day: TICKS_PER_DAY};
