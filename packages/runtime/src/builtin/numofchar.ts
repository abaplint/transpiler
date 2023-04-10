import {Integer} from "../types";
import {ICharacter} from "../types/_character";

export function numofchar(input: {val: ICharacter | string}): Integer {
  let str = "";
  if (typeof input.val === "string") {
    str = input.val;
  } else {
    str = input.val.get();
  }
  str = str.trimEnd();
  return new Integer().set(str.length);
}