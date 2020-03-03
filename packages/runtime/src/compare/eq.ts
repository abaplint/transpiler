import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function eq(left: number | string | ICharacter | INumeric, right: number | string | ICharacter | INumeric) {
  let l = "";
  if (typeof left === "number" || typeof left === "string") {
    l = left.toString();
  } else {
    l = left.get().toString();
  }

  let r = "";
  if (typeof right === "number" || typeof right === "string") {
    r = right.toString();
  } else {
    r = right.get().toString();
  }

  return l === r;
}