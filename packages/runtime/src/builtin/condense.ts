import {String} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function condense(input: {val: ICharacter | INumeric | string}) {
  let str = "";
  if (typeof input.val === "string") {
    str = input.val;
  } else {
    str = input.val.get().toString();
  }

  str = str.replace(/ +$/, "");
  str = str.replace(/^ +/, "");

  return new String().set(str.replace(/ {2,}/g," "));
}