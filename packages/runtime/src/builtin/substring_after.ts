import {ABAPRegExp} from "../abap_regex";
import {Character} from "../types";
import {ICharacter} from "../types/_character";
import {String} from "../types/string";

export function substring_after(input: {val: ICharacter | string, sub?: ICharacter | string, regex?: ICharacter | string}): ICharacter {
  let val = typeof input.val === "string" ? input.val : input.val.get();
  if (input.val instanceof Character) {
    val = input.val.getTrimEnd();
  }
  let reg = "";
  if (typeof input.regex === "string") {
    reg = input.regex;
  } else if (input?.regex) {
    reg = input.regex.get();
  } else if (typeof input.sub === "string") {
    reg = ABAPRegExp.escapeRegExp(input.sub);
  } else if (input?.sub) {
    reg = ABAPRegExp.escapeRegExp(input.sub.get());
  }

  const r = new RegExp(reg + "(.*)");
  const res = val.match(r);

  let ret = "";
  if (res && res[1]) {
    ret = res[1];
  }
  return new String().set(ret);
}