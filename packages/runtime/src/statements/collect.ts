import {eq} from "../compare/index.js";
import {Table} from "../types/index.js";
import {ICharacter} from "../types/_character.js";
import {insertInternal} from "./insert_internal.js";
import {readTable} from "./read_table.js";

export function collect(source: ICharacter, target: Table) {
  const read = readTable(target, {withKey: (i) => {return eq(i.table_line, source);}});
  if (read.subrc === 4) {
    insertInternal({table: target, data: source});
  }
}