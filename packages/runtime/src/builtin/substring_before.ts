import {ICharacter} from "../types/_character";
import {String} from "../types/string";

export function substring_before(input: {val: ICharacter | string, regex: ICharacter | string}): ICharacter {
  const val = typeof input.val === "string" ? input.val : input.val.get();
  const reg = typeof input.regex === "string" ? input.regex : input.regex.get();

  const r = new RegExp("(.*?)" + reg);
  const res = val.match(r);

  let ret = "";
  if (res && res[1]) {
    ret = res[1];
  }
  return new String().set(ret);
}