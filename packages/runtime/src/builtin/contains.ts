import {String} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export type ContainsInput = {
  val: ICharacter
  regex?: ICharacter
  sub?: ICharacter
  start?: ICharacter
  end?: ICharacter
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

  let ret = " ";
  if (input.regex) {
    ret = input.val.get().match(input.regex.get()) !== null ? "X" : " ";
  } else if (input.sub) {
    ret = input.val.get().includes(input.sub.get()) ? "X" : " ";
  } else if (input.start) {
    ret = input.val.get().startsWith(input.start.get()) ? "X" : " ";
  } else if (input.end) {
    ret = input.val.get().endsWith(input.end.get()) ? "X" : " ";
  }

  return new String().set(ret);
}