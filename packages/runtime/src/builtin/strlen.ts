import {Integer} from "../types";
import {ICharacter} from "../types/_character";

export function strlen(input: ICharacter | string): Integer {
  let str = "";
  if (typeof input === "string") {
    str = input;
  } else {
    str = input.get();
  }
  return new Integer().set(str.length);
}