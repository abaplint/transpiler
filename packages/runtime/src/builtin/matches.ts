import {ICharacter} from "../types/_character.js";
import {Character} from "../types/index.js";

export function matches(input: {val: ICharacter | string, regex?: ICharacter | string, pcre?: ICharacter | string}): ICharacter {
  if (input.pcre !== undefined) {
    throw "matches(), todo, pcre";
  } else if (input.regex === undefined) {
    throw "matches(), regex input expected";
  }

  const val = typeof input.val === "string" ? input.val : input.val.get();
  let reg = "";
  if (typeof input.regex === "string") {
    reg = input.regex;
  } else {
    reg = input.regex.get();
  }

  const r = new RegExp("^" + reg + "$");
  const res = val.match(r);

  if (res !== null) {
    return new Character().set("X");
  } else {
    return new Character().set(" ");
  }

}