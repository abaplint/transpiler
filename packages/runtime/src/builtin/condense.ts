import {String} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function condense(input: {val: ICharacter | INumeric}) {
  return new String().set(input.val.get().toString().trim().replace(/\s{2,}/g," "));
}