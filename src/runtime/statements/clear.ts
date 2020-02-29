import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {Table} from "../types";

export function clear(value: ICharacter | INumeric | Table) {
  value.clear();
}