import {Integer} from "../types/index.js";
import {ICharacter} from "../types/_character.js";

export function xstrlen(input: {val: ICharacter | string}): Integer {
  if (typeof input.val === "string") {
    return new Integer().set(input.val.length / 2);
  } else {
    return new Integer().set(input.val.get().length / 2);
  }
}