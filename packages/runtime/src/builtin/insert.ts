import {String} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export interface IInsertInput {
  val: ICharacter;
  sub: ICharacter;
  off?: INumeric;
}

export function insert(input: IInsertInput) {
  let value = input.val.get();

  let offset = 0;
  if (input.off) {
    offset = input.off.get();
  }

  value = value.substring(0, offset) + input.sub.get() + value.substring(offset);

  return new String().set(value);
}