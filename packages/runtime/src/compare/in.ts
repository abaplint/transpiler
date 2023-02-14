import {Table} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {eq} from "./eq";

export function compareIn(left: number | string | ICharacter | INumeric, right: Table): boolean {
  if (right.array().length === 0) {
    return true;
  }

  for (const row of right.array()) {
    if (eq(row.get()["sign"], "I") && eq(row.get()["option"], "EQ")) {
      return eq(row.get()["low"], left);
    } else {
      throw "compareIn todo";
    }
  }

  return false;
}