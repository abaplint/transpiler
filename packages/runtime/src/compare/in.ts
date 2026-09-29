import {Table} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {cp} from "./cp";
import {eq} from "./eq";
import {ge} from "./ge";
import {gt} from "./gt";
import {le} from "./le";
import {lt} from "./lt";
import {ne} from "./ne";

type Operand = number | string | ICharacter | INumeric;

function matches(left: Operand, option: string, low: any, high: any): boolean {
  switch (option) {
    case "EQ": return eq(left, low);
    case "NE": return ne(left, low);
    case "GT": return gt(left, low);
    case "GE": return ge(left, low);
    case "LT": return lt(left, low);
    case "LE": return le(left, low);
    case "BT": return ge(left, low) && le(left, high);
    case "NB": return !(ge(left, low) && le(left, high));
    case "CP": return cp(left, low);
    case "NP": return !cp(left, low);
    default: throw new Error("compareIn, unknown option " + option);
  }
}

// A value is in a range table when some I row admits it, or there is no I
// row at all, and no E row admits it. An empty table admits everything.
export function compareIn(left: Operand, right: Table): boolean {
  let included = false;
  let hasInclude = false;

  for (const row of right.array()) {
    const r = row.get();
    const sign = r["sign"].get().toString().toUpperCase();
    const option = r["option"].get().toString().toUpperCase().trim();
    const hit = matches(left, option, r["low"], r["high"]);
    if (sign === "I") {
      hasInclude = true;
      included = included || hit;
    } else if (sign === "E") {
      if (hit) {
        return false;
      }
    } else {
      throw new Error("compareIn, unknown sign " + sign);
    }
  }

  return hasInclude === false || included;
}
