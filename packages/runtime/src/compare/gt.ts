import {ABAPObject, Structure, Table} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function gt(
  left: number | string | ICharacter | INumeric | ABAPObject | Structure | Table,
  right: number | string | ICharacter | INumeric | ABAPObject | Structure | Table) {

  if (left instanceof Table || right instanceof Table) {
    throw "runtime_todo, gt TABLE";
  }

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

  if (l === undefined) {
    return true; // todo, not sure this is correct
  }
  if (r === undefined) {
    return true; // todo, not sure this is correct
  }

  return l > r;
}