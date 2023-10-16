/* eslint-disable default-case */
import {Date,Time,Hex, Float, Integer, DecFloat34} from "../types";
import {XString} from "../types/xstring";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function parse(val: INumeric | ICharacter | string | number | Float | DecFloat34 | Integer): number {
  switch (typeof val) {
    case "number":
      return val;
    case "string":
      if (val.includes(".")) {
        return parseFloat(val);
      } else {
        return parseInt(val, 10);
      }
    case "object":
      switch (val.constructor) {
        case Float:
          return (val as Float).getRaw();
        case XString:
        case Hex:
          if (val.get() === "") {
            return 0;
          }
          {
            let num = parseInt((val as XString | Hex).get(), 16);
      // handle two complement,
            if (val instanceof Hex && val.getLength() >= 4) {
              const maxVal = Math.pow(2, val.get().length / 2 * 8);
              if (num > maxVal / 2 - 1) {
                num = num - maxVal;
              }
            }
            return num;
          }
        case Time:
        case Date:
          return (val as Time | Date).getNumeric();
        case DecFloat34:
          return (val as DecFloat34).getRaw();
      }
  }

  return parse(val.get());

}