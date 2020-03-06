import {INumeric} from "../types/_numeric";
import {ICharacter} from "../types/_character";
import {Table} from "../types";

export function append(input: {source: number | string | INumeric | ICharacter | Table, target: Table}) {
  input.target.append(input.source);
}