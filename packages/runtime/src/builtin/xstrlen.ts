import {Hex, Integer, XString} from "../types";
import {ICharacter} from "../types/_character";

export function xstrlen(input: {val: ICharacter | string | Hex | XString}): Integer {
  if (typeof input.val === "string") {
    return new Integer().set(input.val.length / 2);
  } else {
    return new Integer().set(input.val.get().length / 2);
  }
}