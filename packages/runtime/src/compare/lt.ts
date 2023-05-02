import {ABAPObject, Structure, Table} from "../types/index.js";
import {ICharacter} from "../types/_character.js";
import {INumeric} from "../types/_numeric.js";
import {gt} from "./gt.js";

export function lt(
  left: number | string | ICharacter | INumeric | ABAPObject | Structure | Table,
  right: number | string | ICharacter | INumeric | ABAPObject | Structure | Table) {
  return gt(right, left);
}