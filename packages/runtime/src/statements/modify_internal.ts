import {Table} from "../types";
import {INumeric} from "../types/_numeric";

export interface IModifyInternalOptions {
  index: INumeric,
  from: any,
}

export function modifyInternal(table: Table, options: IModifyInternalOptions): void {

  const index = options.index.get() - 1;

  const found = table.array()[index] !== undefined;
  if (found) {
    table.array()[index] = options.from;
  }

  const subrc = found ? 0 : 4;
  // @ts-ignore
  abap.builtin.sy.get().subrc.set(subrc);
}