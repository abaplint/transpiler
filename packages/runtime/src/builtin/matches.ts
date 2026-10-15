import {ICharacter} from "../types/_character";
import {Character} from "../types";
import {ABAPRegExp} from "../abap_regex";

export function matches(input: {val: ICharacter | string, regex?: ICharacter | string, pcre?: ICharacter | string}): ICharacter {
  if (input.pcre === undefined && input.regex === undefined) {
    throw new Error("matches(), todo");
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

  // the whole text must match, also when the pattern has alternatives
  const r = ABAPRegExp.create("^(?:" + reg + ")$", "");
  const res = val.match(r);

  if (res !== null) {
    return new Character().set("X");
  } else {
    return new Character().set(" ");
  }

}