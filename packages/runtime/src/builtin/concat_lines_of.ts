import {Table} from "../types";
import {ICharacter} from "../types/_character";

export function concat_lines_of(input: {table: Table, sep: ICharacter | string | undefined}): string {
  let s = input.sep;
  if (s === undefined) {
    s = "";
  } else if (typeof s !== "string") {
    s = s.get();
  }

  return input.table.array().map(e => e.get()).join(s);
}