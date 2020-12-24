import {ICharacter} from "../types/_character";
import {String} from "../types/string";

export function shift_left(input: {val: ICharacter | string, sub: ICharacter | string}): ICharacter {
  let val = typeof input.val === "string" ? input.val : input.val.get();
  const sub = typeof input.sub === "string" ? input.sub : input.sub.get();

  while(val.startsWith(sub)) {
    val = val.substr(sub.length);
  }

  return new String().set(val);
}