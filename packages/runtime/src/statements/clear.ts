import {ICharacter} from "../types/_character.js";
import {INumeric} from "../types/_numeric.js";
import {Table} from "../types/index.js";

export function clear(value: ICharacter | INumeric | Table) {
  value.clear();
}