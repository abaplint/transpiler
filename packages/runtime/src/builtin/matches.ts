import {ICharacter} from "../types/_character";
import {Character} from "../types";

export function matches(input: {val: ICharacter | string, regex?: ICharacter | string, pcre?: ICharacter | string}): ICharacter {
  if (input.pcre === undefined && input.regex === undefined) {
    throw "matches(), todo";
  }

  const val = typeof input.val === "string" ? input.val : input.val.get();
  let reg = "";
  if (input.regex) {
    if (typeof input.regex === "string") {
      reg = input.regex;
    } else {
      reg = input.regex.get();
    }
  } else if (input.pcre) {
    if (typeof input.pcre === "string") {
      reg = input.pcre;
    } else {
      reg = input.pcre.get();
    }
  }

  const r = new RegExp("^" + reg + "$");
  const res = val.match(r);

  if (res !== null) {
    return new Character().set("X");
  } else {
    return new Character().set(" ");
  }

}