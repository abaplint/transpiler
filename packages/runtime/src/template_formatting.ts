import {alphaIn, alphaOut} from "./alpha";
import {ABAPObject, Character, DecFloat34, FieldSymbol, Float, HashedTable, Integer, Packed, Structure, Table} from "./types";
import {ICharacter} from "./types/_character";
import {INumeric} from "./types/_numeric";

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

export function templateFormatting(source: ICharacter | INumeric, options?: options): string {
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
  } else if (source instanceof Character) {
    text = source.getTrimEnd();
  } else if (source instanceof DecFloat34) {
    const raw = source.getRaw();
    if (Number.isInteger(raw)) {
      text = raw.toFixed(0);
    } else {
      text = raw + "";
    }
  } else if (source instanceof Float) {
    const raw = source.getRaw();
    if (options?.style === "scientific") {
      text = raw.toExponential().toUpperCase();
      text = text.replace(/([+-])(\d)$/, "$10$2");
    } else if (Number.isInteger(raw)) {
      text = raw.toFixed(0);
    } else {
      text = raw.toFixed(16);
    }
  } else if (source instanceof Packed) {
    if (options?.decimals) {
      text = source.get().toFixed(options.decimals);
    } else {
      text = source.get().toFixed(source.getDecimals());
    }
  } else {
    text = source.get() + "";
  }

  if (options?.alpha === "out") {
    text = alphaOut(source);
  } else if (options?.alpha === "in") {
    text = alphaIn(source, options.alphaInContext);
  }

  if (options) {
    if (options.currency !== undefined) {
      throw "template formatting with currency not supported";
    }
    if (options.date === "iso") {
      text = text.substr(0,4) + "-" + text.substr(4,2) + "-" + text.substr(6,2);
    }
    if (options.time === "iso") {
      text = text.substr(0,2) + ":" + text.substr(2,2) + ":" + text.substr(4,2);
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
        text = text.trimEnd().padStart(options.width, options.pad);
      } else {
        text = text.trimEnd().padEnd(options.width, options.pad);
      }
    } else if (options.width) {
      text = text.trimEnd().padEnd(options.width, " ");
    } else if (options.decimals && source instanceof Integer) {
      text = source.get().toFixed(options.decimals);
    } else if (options.decimals && source instanceof Float) {
      text = source.getRaw().toFixed(options.decimals);
    }
  }

  return text;
}