import {Table} from "../types";
import {INumeric} from "../types/_numeric";

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
  } else {
// with table key
// todo
  }

  const subrc = found ? 0 : 4;
  // @ts-ignore
  abap.builtin.sy.get().subrc.set(subrc);
}