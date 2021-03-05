import {ICharacter} from "../types/_character";
import {String} from "../types/string";

export function substring_after(input: {val: ICharacter | string, sub: ICharacter | string}): ICharacter {
  const val = typeof input.val === "string" ? input.val : input.val.get();
  const s = typeof input.sub === "string" ? input.sub : input.sub.get();

  const r = new RegExp(s + "(.*)");
  const res = val.match(r);

  let ret = "";
  if (res && res[1]) {
    ret = res[1];
  }
  return new String().set(ret);
}