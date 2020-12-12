import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function co(left: number | string | ICharacter | INumeric, right: string | ICharacter): boolean {
  let l = "";
  if (typeof left === "number" || typeof left === "string") {
    l = left.toString();
  } else {
    l = left.get().toString();
  }

  let r = "";
  if (typeof right === "string") {
    r = right.toString();
  } else {
    r = right.get().toString();
  }

  const characters = r.split("");

  return l.split("").every(c => characters.includes(c));
}