import {Integer} from "../types";
import {ICharacter} from "../types/_character";

export function strlen(input: ICharacter): Integer {
  return new Integer().set(input.get().length);
}