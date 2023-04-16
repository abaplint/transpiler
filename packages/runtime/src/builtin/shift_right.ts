import {ICharacter} from "../types/_character";
import {String} from "../types/string";
import {INumeric} from "../types/_numeric";
// import {throwError} from "../throw_error";

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