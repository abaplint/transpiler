import {eq} from "../compare";
import {Table} from "../types";
import {ICharacter} from "../types/_character";
import {insertInternal} from "./insert_internal";
import {readTable} from "./read_table";

export function collect(source: ICharacter | Table, target?: Table) {
  if (target === undefined && source instanceof Table) {
// with header line
    const read = readTable(source, {withKey: (i) => {return eq(i.table_line, source.getHeader());}});
    if (read.subrc === 4) {
      insertInternal({table: source, data: source.getHeader()});
    }
  } else if (target !== undefined) {
    const read = readTable(target, {withKey: (i) => {return eq(i.table_line, source);}});
    if (read.subrc === 4) {
      insertInternal({table: target, data: source});
    }
  } else {
    throw "COLLECT, no target specified";
  }
}