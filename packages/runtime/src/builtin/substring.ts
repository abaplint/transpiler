import {ICharacter} from "../types/_character";
import {String} from "../types/string";
import {INumeric} from "../types/_numeric";

export interface ISubstringInput {
  val: ICharacter | string;
  off?: INumeric;
  len?: INumeric;
}

export function substring(input: ISubstringInput): ICharacter {
  const val = typeof input.val === "string" ? input.val : input.val.get();
  let off = input?.off?.get();
  if (off === undefined) {
    off = 0;
  }
  const len = input?.len?.get();

  const sub = val.substr(off, len);
  return new String().set(sub);
}