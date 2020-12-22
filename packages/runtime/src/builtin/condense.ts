import {String} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function condense(input: ICharacter | INumeric) {
  return new String().set(input.get().toString().trim().replace(/\s{2,}/g," "));
}