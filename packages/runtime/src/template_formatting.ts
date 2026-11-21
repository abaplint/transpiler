import {alphaIn, alphaOut} from "./alpha";
import {ABAPObject, Character, DecFloat34, FieldSymbol, Float, HashedTable, Integer, Packed, String, Structure, Table} from "./types";
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
  currency?: any,
  style?: any,
  alpha?: "out" | "in",
  alphaInContext?: any,
  align?: "left" | "right",
};

// an f that is not a whole number is written with 17 significant digits, without trailing zeros
// and without an exponent, as on a system: 0.25 is 0.25, 0.1 is 0.10000000000000001
function floatToText(raw: number): string {
  const [mantissa, exponentText] = raw.toPrecision(17).toLowerCase().split("e");
  const exponent = exponentText === undefined ? 0 : parseInt(exponentText, 10);
  const negative = mantissa.startsWith("-");
  const [intPart, fracPart = ""] = (negative ? mantissa.slice(1) : mantissa).split(".");
  let digits = intPart + fracPart;
  let point = intPart.length + exponent;
  if (point <= 0) {
    digits = "0".repeat(1 - point) + digits;
    point = 1;
  } else if (point > digits.length) {
    digits = digits.padEnd(point, "0");
  }
  const text = (digits.slice(0, point) + "." + digits.slice(point))
    .replace(/0+$/, "")
    .replace(/\.$/, "")
    .replace(/^0+(?=\d)/, "");
  return (negative ? "-" : "") + text;
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
  } else if (source instanceof DecFloat34) {
    // exact, the value has up to 34 digits
    if (options?.decimals !== undefined) {
      text = decimalToFixed(source.getDecimal(), options.decimals);
    } else {
      text = source.toText();
    }
  } else if (source instanceof Float) {
    const raw = source.getRaw();
    if (options?.style === "scientific") {
      text = raw.toExponential().toUpperCase();
      text = text.replace(/([+-])(\d)$/, "$10$2");
    } else if (Number.isInteger(raw)) {
      text = raw.toFixed(0);
    } else {
      text = floatToText(raw);
    }
  } else if (source instanceof Packed) {
    if (options?.decimals !== undefined) {
      text = source.toFixed(options.decimals);
    } else {
      text = source.toFixed(source.getDecimals());
    }
  } else if (typeof source === "number" || typeof source === "string") {
    text = source + "";
  } else {
    text = source.get() + "";
  }

  if (options?.alpha === "out") {
    if (typeof source === "number" || typeof source === "string") {
      text = text.replace(/^0+/, "");
    } else {
      text = alphaOut(source);
    }
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
    } else if (options.width && options.pad) {
      if (options.align === "right") {
        text = text.padStart(options.width, options.pad);
      } else {
        text = text.padEnd(options.width, options.pad);
      }
    } else if (options.width) {
      text = text.padEnd(options.width, " ");
    } else if (options.decimals !== undefined && source instanceof Integer) {
      text = source.get().toFixed(options.decimals);
    } else if (options.decimals !== undefined && source instanceof Float) {
      text = source.getRaw().toFixed(options.decimals);
    }
  }

  return text;
}
