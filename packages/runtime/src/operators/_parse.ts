import {Date,Time,Hex} from "../types";
import {XString} from "../types/xstring";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function parse(val: INumeric | ICharacter | string | number): number {
  if (typeof val === "number") {
    return val;
  } else if (typeof val === "string") {
    return parseInt(val, 10);
  } else if (val instanceof XString || val instanceof Hex) {
    return parseInt(val.get(), 16);
  } else if (val instanceof Time || val instanceof Date) {
    return val.getNumeric();
  } else {
    return parse(val.get());
  }
}