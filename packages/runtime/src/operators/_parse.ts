import {Date,Time,Hex, Float, Integer} from "../types";
import {XString} from "../types/xstring";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function parse(val: INumeric | ICharacter | string | number | Float | Integer): number {
  if (typeof val === "number") {
    return val;
  } else if (typeof val === "string") {
    return parseInt(val, 10);
  } else if (val instanceof Float) {
    return val.getRaw();
  } else if (val instanceof XString || val instanceof Hex) {
    if (val.get() === "") {
      return 0;
    }
    return parseInt(val.get(), 16);
  } else if (val instanceof Time || val instanceof Date) {
    return val.getNumeric();
  } else {
    return parse(val.get());
  }
}