import {Table} from "../types/index.js";
import {ICharacter} from "../types/_character.js";
import {INumeric} from "../types/_numeric.js";
import {cp} from "./cp.js";
import {eq} from "./eq.js";
import {ne} from "./ne.js";

export function compareIn(left: number | string | ICharacter | INumeric, right: Table): boolean {
  if (right.array().length === 0) {
    return true;
  }

  for (const row of right.array()) {
    if (eq(row.get()["sign"], "I") && eq(row.get()["option"], "EQ")) {
      if (eq(row.get()["low"], left)) {
        return true;
      }
    } else if (eq(row.get()["sign"], "E") && eq(row.get()["option"], "EQ")) {
      if (ne(row.get()["low"], left)) {
        return true;
      }
    } else if (eq(row.get()["sign"], "I") && eq(row.get()["option"], "CP")) {
      if (cp(left, row.get()["low"])) {
        return true;
      }
    } else {
      console.dir(row);
      throw new Error("compareIn todo");
    }
  }

  return false;
}