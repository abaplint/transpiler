import {Integer} from "../types";
import {ICharacter} from "../types/_character";

export function xstrlen(input: ICharacter | string): Integer {
  if (typeof input === "string") {
    return new Integer().set(input.length / 2);
  } else {
    return new Integer().set(input.get().length / 2);
  }
}