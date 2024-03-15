import {Date,Time,Hex, Float, Integer, DecFloat34, HexUInt8} from "../types";
import {XString} from "../types/xstring";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function parse(val: INumeric | ICharacter | string | number | Float | Integer): number {
  if (typeof val === "number") {
    return val;
  } else if (typeof val === "string") {
    if (val.includes(".")) {
      return parseFloat(val);
    } else {
      return parseInt(val, 10);
    }
  } else if (val instanceof Integer) {
    // optimize, as this is the most common case
    return val.get();
  } else if (val instanceof Float) {
    return val.getRaw();
  } else if (val instanceof XString || val instanceof Hex || val instanceof HexUInt8) {
    if (val.get() === "") {
      return 0;
    }
    let num = parseInt(val.get(), 16);
// handle two complement,
    if (val instanceof Hex && val.getLength() >= 4) {
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
  } else {
    return parse(val.get());
  }
}