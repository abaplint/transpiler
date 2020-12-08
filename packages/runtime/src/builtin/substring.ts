import {ICharacter} from "../types/_character";
import {String} from "../types/string";
import {INumeric} from "../types/_numeric";

export function substring(input: {val: ICharacter | string, off: INumeric, len: INumeric}): ICharacter {
  const val = typeof input.val === "string" ? input.val : input.val.get();
  const off = input.off.get();
  const len = input.len.get();

  const sub = val.substr(off, len);

  return new String().set(sub);
}