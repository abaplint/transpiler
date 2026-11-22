import {alphaIn, alphaOutTemplate} from "./alpha";
import {ABAPObject, Character, DecFloat34, FieldSymbol, Float, HashedTable, Integer, Integer8, Numc, Packed, String, Structure, Table} from "./types";
import {ICharacter} from "./types/_character";
import {INumeric} from "./types/_numeric";
import {decimalToFixed} from "./types/_decimal";

type options = {
  timestamp?: string,
  date?: string,
  time?: string,
  pad?: string,
  width?: number,
  decimals?: number,
  exponent?: number,
  currency?: any,
  style?: any,
  alpha?: "out" | "in",
  alphaInContext?: any,
  align?: "left" | "right" | "center",
  sign?: "left" | "leftplus" | "leftspace" | "right" | "rightplus" | "rightspace",
};

/** an exact decimal number: the value is 0.digits * 10^point, digits has no leading or trailing
 * zeros, it is empty for zero */
type DecimalDigits = {negative: boolean, digits: string, point: number};

/** from plain decimal text, ie. "-1234567.89", or a JS number text with an exponent, "1.5e-200" */
function toDigits(text: string): DecimalDigits {
  const [mantissa, exponentText] = text.toLowerCase().split("e");
  const negative = mantissa.startsWith("-");
  const [integer, fraction = ""] = (negative ? mantissa.substring(1) : mantissa).split(".");
  let digits = integer + fraction;
  let point = integer.length + (exponentText === undefined ? 0 : parseInt(exponentText, 10));
  const leading = digits.length - digits.replace(/^0+/, "").length;
  digits = digits.substring(leading).replace(/0+$/, "");
  point = digits === "" ? 0 : point - leading;
  return {negative, digits, point};
}

/** an f has 17 significant digits, ie. 0.1 is 0.10000000000000001 */
function floatDigits(raw: number): DecimalDigits {
  return toDigits(raw.toPrecision(17));
}

/** rounded half away from zero to the given number of decimals */
function roundDigits(value: DecimalDigits, decimals: number): DecimalDigits {
  const keep = value.point + decimals;
  if (keep >= value.digits.length) {
    return value;
  } else if (keep < 0) {
    return {negative: value.negative, digits: "", point: 0};
  }
  let digits = value.digits.substring(0, keep);
  let point = value.point;
  if (value.digits[keep] >= "5") {
    const up = (BigInt("1" + digits) + 1n).toString();
    if (up[0] === "2") {
      digits = "1" + up.substring(1);
      point++;
    } else {
      digits = up.substring(1);
    }
  }
  const trimmed = digits.replace(/0+$/, "");
  return {negative: value.negative, digits: trimmed, point: trimmed === "" ? 0 : point};
}

/** plain text, without an exponent, with at least the given number of decimals */
function digitsToText(value: DecimalDigits, decimals = 0): string {
  let integer = "0";
  let fraction = "";
  if (value.digits !== "" && value.point > 0) {
    integer = value.digits.substring(0, value.point).padEnd(value.point, "0");
    fraction = value.digits.substring(value.point);
  } else if (value.digits !== "") {
    fraction = "0".repeat(-value.point) + value.digits;
  }
  fraction = fraction.padEnd(decimals, "0");
  return (value.negative ? "-" : "") + integer + (fraction === "" ? "" : "." + fraction);
}

/** DECIMALS, an f is written with at most 17 digits */
function floatDecimals(value: DecimalDigits, decimals: number): string {
  const allowed = Math.min(decimals, Math.max(0, 17 - Math.max(value.point, 1)));
  return digitsToText(roundDigits(value, allowed), allowed);
}

/** the value written as a mantissa times ten to the exponent, a sign and at least two exponent digits */
function withExponent(value: DecimalDigits, exponent: number, decimals?: number, floatLimit = false): string {
  const mantissa = {...value, point: value.point - exponent};
  let text = "";
  if (decimals === undefined) {
    text = digitsToText(mantissa);
  } else if (floatLimit) {
    text = floatDecimals(mantissa, decimals);
  } else {
    text = digitsToText(roundDigits(mantissa, decimals), decimals);
  }
  return text + "E" + (exponent < 0 ? "-" : "+") + Math.abs(exponent).toString().padStart(2, "0");
}

/** STYLE = SCIENTIFIC, one digit before the decimal point, ie. "-1.23456789E+06" */
function scientific(value: DecimalDigits, decimals?: number): string {
  const exponent = value.digits === "" ? 0 : value.point - 1;
  if (decimals !== undefined && value.digits !== "") {
    const rounded = roundDigits({...value, point: 1}, decimals);
    if (rounded.point > 1) {
      // rounded up to ten, the point moves and the digits stay: 9.99995 to 2 decimals is 1.000E+03
      return withExponent({...rounded, point: rounded.point + exponent}, exponent + 1, decimals + 1);
    }
  }
  return withExponent(value, exponent, decimals);
}

/** STYLE = ENGINEERING, the exponent is a multiple of three, one to three digits before the decimal point */
function engineering(value: DecimalDigits, decimals?: number): string {
  let exponent = value.digits === "" ? 0 : Math.floor((value.point - 1) / 3) * 3;
  if (decimals !== undefined && roundDigits({...value, point: value.point - exponent}, decimals).point > 3) {
    exponent += 3;
  }
  return withExponent(value, exponent, decimals);
}

/** SIGN: where the sign goes, and whether a positive number gets a plus sign */
function applySign(text: string, sign: string): string {
  const negative = text.startsWith("-");
  const absolute = negative ? text.substring(1) : text;
  switch (sign) {
    case "leftplus":
      return (negative ? "-" : "+") + absolute;
    case "right":
      return absolute + (negative ? "-" : "");
    case "rightplus":
      return absolute + (negative ? "-" : "+");
    default:
      return text;
  }
}

function isNumeric(source: unknown): boolean {
  return source instanceof Integer
    || source instanceof Integer8
    || source instanceof Packed
    || source instanceof Float
    || source instanceof DecFloat34;
}

export function templateFormatting(source: ICharacter | INumeric | number | string, options?: options): string {
  let text = "";

  if (source instanceof FieldSymbol) {
    if (source.getPointer() === undefined) {
      throw new Error("GETWA_NOT_ASSIGNED");
    }
    return templateFormatting(source.getPointer(), options);
  } else if (source instanceof Table
      || source instanceof HashedTable
      || source instanceof ABAPObject
      || source instanceof Structure) {
    throw new Error("STRG_ILLEGAL_DATA_TYPE");
  } else if (source instanceof String && source.getEnumName() !== undefined) {
    // an enumerated value is written as the name of its constant
    text = source.getEnumName() + "";
  } else if (source instanceof Character) {
    text = source.getTrimEnd();
  } else if (source instanceof Numc) {
    // after a character statement n can end with blanks, they are dropped as for c
    text = source.get().trimEnd();
  } else if (source instanceof DecFloat34) {
    // exact, the value has up to 34 digits
    if (options?.decimals !== undefined) {
      text = decimalToFixed(source.getDecimal(), options.decimals);
    } else {
      text = source.toText();
    }
  } else if (source instanceof Float) {
    // 17 significant digits, without trailing zeros
    const value = floatDigits(source.getRaw());
    const exponent = options?.exponent;
    if (exponent !== undefined && (exponent !== 0 || options?.style === "scientific")) {
      text = withExponent(value, exponent, options?.decimals, true);
    } else if (options?.style === "scientific") {
      text = scientific(value, options?.decimals);
    } else if (options?.style === "engineering") {
      text = engineering(value, options?.decimals);
    } else if (options?.decimals !== undefined) {
      text = floatDecimals(value, options.decimals);
    } else {
      text = digitsToText(value);
    }
  } else if (source instanceof Packed) {
    if (options?.decimals !== undefined) {
      text = source.toFixed(options.decimals);
    } else if (options?.style === "scientific") {
      text = scientific(toDigits(source.toFixed(source.getDecimals())));
    } else if (options?.style === "engineering") {
      text = engineering(toDigits(source.toFixed(source.getDecimals())));
    } else {
      text = source.toFixed(source.getDecimals());
    }
  } else if (source instanceof Integer) {
    if (options?.decimals !== undefined) {
      text = source.get().toFixed(options.decimals);
    } else if (options?.style === "scientific") {
      text = scientific(toDigits(source.get() + ""));
    } else if (options?.style === "engineering") {
      text = engineering(toDigits(source.get() + ""));
    } else {
      text = source.get() + "";
    }
  } else if (source instanceof Integer8 && options?.style === "scientific") {
    text = scientific(toDigits(source.get() + ""));
  } else if (typeof source === "number" || typeof source === "string") {
    text = source + "";
  } else {
    text = source.get() + "";
  }

  if (options?.alpha === "out") {
    // the length of the result is WIDTH, else the field length for c and n, else the length of the text
    let length = source instanceof Character || source instanceof Numc ? source.getLength() : text.length;
    if (options.width) {
      length = options.width;
    }
    text = alphaOutTemplate(text, length);
  } else if (options?.alpha === "in") {
    if (typeof source !== "number" && typeof source !== "string") {
      text = alphaIn(source, options.alphaInContext);
    }
    if (options.width && text.match(/^[0-9 ]+$/) !== null && text.trim() !== "") {
      // with WIDTH, the digits are padded with zeros to the width
      text = text.trim().padStart(options.width, "0");
    }
  }

  if (options) {
    if (options.currency !== undefined) {
      throw new Error("template formatting with currency not supported");
    }
    if (options.date === "iso") {
        text = text.substr(0, 4) + "-" + text.substr(4, 2) + "-" + text.substr(6, 2);
    } else if (options.date === "user" || options.date === "environment") {
        const yyyy = text.substr(0, 4);
        const mm = text.substr(4, 2);
        const dd = text.substr(6, 2);
        const date = new Date(`${yyyy}-${mm}-${dd}T00:00:00`);
        if (isNaN(date.getTime())) {
            text = "00000000";         // Default SAP behaviour
        } else {
            text = new Intl.DateTimeFormat("default",{day: "2-digit", month: "2-digit", year: "numeric"}).format(date);
        }
    }
    if (options.time === "iso") {
        text = text.substr(0, 2) + ":" + text.substr(2, 2) + ":" + text.substr(4, 2);
    } else if (options.time === "user" || options.time === "environment") {
        const hh = text.substr(0, 2);
        const mm = text.substr(2, 2);
        const ss = text.substr(4, 2) || "00";

        const timeDate = new Date(`1970-01-01T${hh}:${mm}:${ss}`);

        if (isNaN(timeDate.getTime())) {
            text = "";         // Default SAP behaviour
        } else {
            // Leaving out hour12 makes it use the exact environment defaults!
            text = new Intl.DateTimeFormat("default", {
                hour: "2-digit",
                minute: "2-digit",
                second: "2-digit",
            }).format(timeDate);
        }
    }
    if (options.timestamp === "iso") {
      // make sure to get decimals from packed number,
      text = templateFormatting(source).replace(".", ",");
      text = text.substr(0,4) + "-" + text.substr(4,2) + "-" + text.substr(6,2) + "T" + text.substr(8,2) + ":" + text.substr(10,2) + ":" + text.substr(12,2) + text.substr(14);
      if (text === "0--T::") {
        text = "0000-00-00T00:00:00";
      }
    } else {
      if (options.sign !== undefined && isNumeric(source)) {
        text = applySign(text, options.sign);
      }
      if (options.width) {
        // ALIGN places the text in the WIDTH, PAD fills the rest, a blank by default
        const pad = options.pad ?? " ";
        const fill = options.width - text.length;
        if (options.align === "right") {
          text = text.padStart(options.width, pad);
        } else if (options.align === "center" && fill > 0) {
          const left = Math.floor(fill / 2);
          text = pad.repeat(left) + text + pad.repeat(fill - left);
        } else {
          text = text.padEnd(options.width, pad);
        }
      }
    }
  }

  return text;
}
