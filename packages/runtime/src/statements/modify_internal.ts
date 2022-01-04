import {Integer, Table} from "../types";
import {INumeric} from "../types/_numeric";
import {deleteInternal} from "./delete_internal";
import {insertInternal} from "./insert_internal";
import {readTable} from "./read_table";

export interface IModifyInternalOptions {
  index: INumeric,
  from: any,
}

export function modifyInternal(table: Table, options: IModifyInternalOptions): void {

  let found = false;

  if (options.index) {
    const index = options.index.get() - 1;
    found = table.array()[index] !== undefined;
    if (found) {
      table.deleteIndex(index);
      table.insertIndex(options.from, index);
    }
  } else if (options.from) {
    const readResult = readTable(table, {from: options.from});
    if (readResult.subrc === 0) {
      deleteInternal(table, {index: new Integer().set(readResult.foundIndex)});
    }
    insertInternal({table, data: options.from});
  }

  const subrc = found ? 0 : 4;
  // @ts-ignore
  abap.builtin.sy.get().subrc.set(subrc);
}