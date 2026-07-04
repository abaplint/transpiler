import {ICharacter} from "../types/_character";
import {String} from "../types/string";

export function match(input: {val: ICharacter | string, regex?: ICharacter | string, pcre?: ICharacter | string}): ICharacter {
  const val = typeof input.val === "string" ? input.val : input.val.get();
  const regexInput = input.pcre || input.regex;
  if (regexInput === undefined) {
    throw new Error("match() requires either regex or pcre parameter");
  }
  let reg = "";
  if (typeof regexInput === "string") {
    reg = regexInput;
  } else {
    reg = regexInput.get();
  }

  const r = new RegExp(reg);
  const res = val.match(r);

  let ret = "";
  if (res && res[0]) {
    ret = res[0];
  }

  return new String().set(ret);
}