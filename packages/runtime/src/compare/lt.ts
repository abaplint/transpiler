import {ABAPObject, Structure, Table} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {gt} from "./gt";

export function lt(
  left: number | string | ICharacter | INumeric | ABAPObject | Structure | Table,
  right: number | string | ICharacter | INumeric | ABAPObject | Structure | Table) {
  return gt(right, left);
}