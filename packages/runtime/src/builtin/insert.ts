import {String} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export interface IInsertInput {
  val: ICharacter;
  sub: ICharacter;
  off?: INumeric;
}

export function insert(input: IInsertInput) {
  let offset = 0;
  if (input.off) {
    offset = input.off.get();
  }

  const value = input.val.getOffset({offset: 0, length: offset}).get() +
                input.sub.get() +
                input.val.getOffset({offset: offset}).get();

  return new String().set(value);
}