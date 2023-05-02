import {ICharacter} from "../types/_character.js";
import {String} from "../types/string.js";
import {INumeric} from "../types/_numeric.js";
// import {throwError} from "../throw_error.js";

export interface IShiftRight {
  val: ICharacter | string,
  sub?: ICharacter | string,
  places?: ICharacter | INumeric | string
  circular?: INumeric;
}

export function shift_right(input: IShiftRight): ICharacter {
  let val = typeof input.val === "string" ? input.val : input.val.get();

  if (input.sub) {
    const sub = typeof input.sub === "string" ? input.sub : input.sub.get();
    while(val.endsWith(sub)) {
      val = val.substr(0, val.length - sub.length);
    }
  } else if (input.places) {
    throw new Error("shift_right todo");
  } else if (input.circular) {
    throw new Error("shift_right todo");
  } else {
    return shift_right({val: input.val, sub: " "});
  }
  return new String().set(val);
}