import {String} from "../types/index.js";
import {ICharacter} from "../types/_character.js";

export function reverse(input: {val: ICharacter | string}) {
  let val = "";
  if (typeof input.val === "string") {
    val = input.val;
  } else {
    val = input.val.get();
  }
  val = val.split("").reverse().join("");
  return new String().set(val);
}