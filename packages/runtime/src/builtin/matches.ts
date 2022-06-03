import {ICharacter} from "../types/_character";
import {Character} from "../types";

export function matches(input: {val: ICharacter | string, regex: ICharacter | string}): ICharacter {
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