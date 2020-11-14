import {String} from "../types";
import {ICharacter} from "../types/_character";

export function condense(input: ICharacter) {
  return new String().set(input.get().trim());
}