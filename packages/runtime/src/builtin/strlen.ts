import {Character, Integer} from "../types/index.js";
import {ICharacter} from "../types/_character.js";

export function strlen(input: {val: ICharacter | string}): Integer {
  let str = "";
  if (typeof input.val === "string") {
    str = input.val;
  } else if (input.val instanceof Character) {
    str = input.val.getTrimEnd();
  } else {
    str = input.val.get();
  }
  return new Integer().set(str.length);
}