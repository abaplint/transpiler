import {Character, String} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function concat(left: INumeric | ICharacter | string | number, right: INumeric | ICharacter | string | number) {
  let val = "";
  if (typeof left === "string" || typeof left === "number") {
    val += left;
  } else if (left instanceof Character) {
    val += left.getTrimEnd();
  } else {
    val += left.get();
  }
  if (typeof right === "string" || typeof right === "number") {
    val += right;
  } else if (right instanceof Character) {
    val += right.getTrimEnd();
  } else {
    val += right.get();
  }
  return new String().set(val);
}