import {Structure, Table} from "../types";
import {eq} from "../compare";
import {INumeric} from "../types/_numeric";
import {loop} from "./loop";

export interface IDeleteInternalOptions {
  where?: (i: any) => boolean,
  index?: INumeric,
  adjacent?: boolean,
  from?: any,
  to?: any,
}

export function deleteInternal(target: Table, options?: IDeleteInternalOptions): void {
  let prev: any = undefined;
  let index = 0;

  if (options?.index
      && options?.where === undefined
      && options?.adjacent === undefined
      && options?.from === undefined
      && options?.to === undefined) {
    target.deleteIndex(options.index.get() - 1);
    return;
  }

  for (const i of loop(target)) {
    // @ts-ignore
    index = abap.builtin.sy.get().tabix.get() - 1;

    if (options?.where) {
      const row = i instanceof Structure ? i.get() : {table_line: i};
      if (options.where(row) === true) {
        target.deleteIndex(index);
      }
    } else if (options?.adjacent === true && prev !== undefined && eq(prev, i) === true) {
      target.deleteIndex(index);
    } else if (options?.index && options.index.get() === index) {
      target.deleteIndex(options.index.get() - 1);
    } else if (options?.from && options.from.get() <= index + 1) {
      target.deleteIndex(index);
    }

    prev = i;
  }
}