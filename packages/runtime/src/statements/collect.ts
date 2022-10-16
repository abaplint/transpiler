import {eq} from "../compare";
import {Table} from "../types";
import {ICharacter} from "../types/_character";
import {insertInternal} from "./insert_internal";
import {readTable} from "./read_table";

export function collect(source: ICharacter, target: Table) {
  const read = readTable(target, {withKey: (i) => {return eq(i.table_line, source);}});
  if (read.subrc === 4) {
    insertInternal({table: target, data: source});
  }
}