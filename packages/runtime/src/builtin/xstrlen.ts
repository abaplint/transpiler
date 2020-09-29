import {Integer} from "../types";
import {ICharacter} from "../types/_character";

export function xstrlen(input: ICharacter): Integer {
  return new Integer().set(input.get().length / 2);
}