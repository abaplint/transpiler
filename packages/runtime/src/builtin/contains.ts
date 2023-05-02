import {String} from "../types/index.js";
import {ICharacter} from "../types/_character.js";
import {INumeric} from "../types/_numeric.js";

export type ContainsInput = {
  val: ICharacter
  regex: ICharacter
  case?: ICharacter,
  off?: INumeric,
  len?: INumeric,
  occ?: INumeric,
};

export function contains(input: ContainsInput) {
  if (input.case !== undefined
      || input.off !== undefined
      || input.len !== undefined
      || input.occ !== undefined) {
    throw "runtime, contains() todo";
  }

  const ret = input.val.get().match(input.regex.get()) !== null ? "X" : " ";
  return new String().set(ret);
}