import {Character, String} from "../types";
import {ICharacter} from "../types/_character";

export function to_upper(input: {val: ICharacter | string}): ICharacter {
  const val = typeof input.val === "string" ? input.val : input.val.get();

  if (input.val instanceof Character) {
    return new Character(input.val.getLength()).set(val.toUpperCase());
  } else {
    return new String().set(val.toUpperCase());
  }
}