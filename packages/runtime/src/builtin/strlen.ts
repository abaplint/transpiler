import {Integer} from "../types";
import {ICharacter} from "../types/_character";

export function strlen(input: {val: ICharacter | string}): Integer {
  let str = "";
  if (typeof input.val === "string") {
    str = input.val;
  } else {
    str = input.val.get();
  }
  return new Integer().set(str.length);
}