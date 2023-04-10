import {Character, String} from "../types";
import {ICharacter} from "../types/_character";

export function to_lower(input: {val: ICharacter | string}): ICharacter {
  const val = typeof input.val === "string" ? input.val : input.val.get();

  if (input.val instanceof Character) {
    return new Character(input.val.getLength()).set(val.toLowerCase());
  } else {
    return new String().set(val.toLowerCase());
  }
}