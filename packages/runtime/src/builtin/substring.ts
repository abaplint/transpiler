import {ICharacter} from "../types/_character";
import {String} from "../types/string";
import {INumeric} from "../types/_numeric";

export interface ISubstringInput {
  val: ICharacter | string;
  off?: INumeric;
  len?: INumeric;
}

export function substring(input: ISubstringInput): ICharacter {
  let off = input?.off?.get();
  if (off === undefined) {
    off = 0;
  }
  const len = input?.len?.get();

  let sub = "";
  if (typeof input.val === "string") {
    sub = input.val.substr(off, len);
  } else {
    sub = input.val.getOffset({offset: off, length: len}).get();
  }
  return new String().set(sub);
}