import {String} from "../types";
import {ICharacter} from "../types/_character";

export function to_upper(input: ICharacter): ICharacter {
  const val = typeof input === "string" ? input : input.get();
  return new String().set(val.toUpperCase());
}