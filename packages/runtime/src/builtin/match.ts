import {ICharacter} from "../types/_character";
import {String} from "../types/string";

export function match(input: {val: ICharacter | string, regex: ICharacter | string}): ICharacter {
  const val = typeof input.val === "string" ? input.val : input.val.get();
  let reg = "";
  if (typeof input.regex === "string") {
    reg = input.regex;
  } else {
    reg = input.regex.get();
  }

  const r = new RegExp(reg);
  const res = val.match(r);

  let ret = "";
  if (res && res[0]) {
    ret = res[0];
  }

  return new String().set(ret);
}