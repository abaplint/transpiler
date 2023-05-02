import {Character, String} from "../types/index.js";
import {ICharacter} from "../types/_character.js";
import {INumeric} from "../types/_numeric.js";

export function concat(left: INumeric | ICharacter | string | number | any[], right: INumeric | ICharacter | string | number): String {
  if (Array.isArray(left)) {
// used in ampersand concatenation
    let res = concat(left[0], left[1]);
    for (let i = 2; i < left.length; i++) {
      res = concat(res, left[i]);
    }
    return res;
  }

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