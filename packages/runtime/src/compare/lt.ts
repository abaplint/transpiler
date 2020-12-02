import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function lt(left: number | string | ICharacter | INumeric, right: number | string | ICharacter | INumeric) {
  let l: number | string | undefined = undefined;
  if (typeof left === "number" || typeof left === "string") {
    l = left;
  } else {
    l = left.get();
  }

  let r: number | string | undefined = undefined;
  if (typeof right === "number" || typeof right === "string") {
    r = right;
  } else {
    r = right.get();
  }

  if (typeof l === "string" && typeof r === "number") {
    r = r.toString();
  }
  if (typeof l === "number" && typeof r === "string") {
    if (r === "") {
      r = 0;
    } else {
      r = parseInt(r, 10);
    }
  }

  return l < r;
}