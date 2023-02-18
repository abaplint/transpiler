import {Character, FieldSymbol, Float, Integer, Packed, Table} from "./types";
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
  align?: "left" | "right",
};

export function templateFormatting(source: ICharacter | INumeric, options?: options) {
  let text = "";
  if (source instanceof FieldSymbol && source.getPointer() === undefined) {
    throw new Error("GETWA_NOT_ASSIGNED");
  } else if (source instanceof Table) {
    throw new Error("STRG_ILLEGAL_DATA_TYPE");
  } else if (source instanceof Character) {
    text = source.getTrimEnd();
  } else {
    text = source.get() + "";
  }

  if (options?.currency !== undefined) {
    throw "template formatting with currency not supported";
  }
  if (options?.timestamp === "iso") {
    text = text.substr(0,4) + "-" + text.substr(4,2) + "-" + text.substr(6,2) + "T" + text.substr(8,2) + ":" + text.substr(10,2) + ":" + text.substr(12,2);
    if (text === "0--T::") {
      text = "0000-00-00T00:00:00";
    }
  }
  if (options?.date === "iso") {
    text = text.substr(0,4) + "-" + text.substr(4,2) + "-" + text.substr(6,2);
  }
  if (options?.time === "iso") {
    text = text.substr(0,2) + ":" + text.substr(2,2) + ":" + text.substr(4,2);
  }
  if (options?.width && options.pad) {
    if (options.align === "right") {
      text = text.trimEnd().padStart(options.width, options.pad);
    } else {
      text = text.trimEnd().padEnd(options.width, options.pad);
    }
  } else if (options?.width) {
    text = text.trimEnd().padEnd(options.width, " ");
  } else if (options?.decimals && source instanceof Integer) {
    text = source.get().toFixed(options.decimals);
  } else if (options?.decimals && source instanceof Packed) {
    text = source.get().toFixed(options.decimals);
  } else if (options?.decimals && source instanceof Float) {
    text = source.getRaw().toFixed(options.decimals);
  } else if (source instanceof Float) {
    const raw = source.getRaw();
    if (Number.isInteger(raw)) {
      text = raw.toFixed(0);
    } else {
      text = raw.toFixed(16);

    }
  }
  return text;
}