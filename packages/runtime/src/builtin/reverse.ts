import {String} from "../types";
import {ICharacter} from "../types/_character";

export function reverse(input: ICharacter | string) {
  let val = "";
  if (typeof input === "string") {
    val = input;
  } else {
    val = input.get();
  }
  val = val.split("").reverse().join("");
  return new String().set(val);
}