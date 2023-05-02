import {Integer, Table} from "../types/index.js";
import {INumeric} from "../types/_numeric.js";
import {deleteInternal} from "./delete_internal.js";
import {insertInternal} from "./insert_internal.js";
import {readTable} from "./read_table.js";

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