import {ICharacter} from "../types/_character";
import {String} from "../types/string";
import {INumeric} from "../types/_numeric";

export function repeat(input: {val: ICharacter | string, occ: INumeric}): ICharacter {
  const val = typeof input.val === "string" ? input.val : input.val.get();
  return new String().set(val.repeat(input.occ.get()));
}