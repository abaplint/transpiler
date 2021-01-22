import {Table} from "../types";
import {ICharacter} from "../types/_character";
import {String} from "../types/string";

export function concat_lines_of(input: {table: Table, sep: ICharacter | string | undefined}): String {
  let s = input.sep;
  if (s === undefined) {
    s = "";
  } else if (typeof s !== "string") {
    s = s.get();
  }

  return new String().set(input.table.array().map(e => e.get()).join(s));
}