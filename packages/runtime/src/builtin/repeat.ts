import {ICharacter} from "../types/_character.js";
import {String} from "../types/string.js";
import {INumeric} from "../types/_numeric.js";

export function repeat(input: {val: ICharacter | string, occ: INumeric}): ICharacter {
  const val = typeof input.val === "string" ? input.val : input.val.get();
  return new String().set(val.repeat(input.occ.get()));
}