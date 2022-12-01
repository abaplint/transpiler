import {ICharacter} from "../types/_character";
import {String} from "../types/string";
import {INumeric} from "../types/_numeric";
import {throwError} from "../throw_error";

export interface IShiftLeftInput {
  val: ICharacter | string,
  sub?: ICharacter | string,
  places?: ICharacter | INumeric | string
  circular?: INumeric;
}

export function shift_left(input: IShiftLeftInput): ICharacter {
  let val = typeof input.val === "string" ? input.val : input.val.get();

  if (input.sub) {
    const sub = typeof input.sub === "string" ? input.sub : input.sub.get();
    while(val.startsWith(sub)) {
      val = val.substr(sub.length);
    }
  } else if (input.places) {
    let places = typeof input.places === "string" ? input.places : input.places.get();
    if (typeof places === "string") {
      places = parseInt(places, 10);
    }
    if (places > val.length) {
      throwError("CX_SY_RANGE_OUT_OF_BOUNDS");
    }
    val = val.substring(places);
  }
  else if (input.circular) {
    const leftShifts = input.circular.get() % val.length;
    val =  val.slice(leftShifts) + val.slice(0, leftShifts);
  }
  return new String().set(val);
}