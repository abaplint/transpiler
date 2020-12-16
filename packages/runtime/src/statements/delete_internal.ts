import {Structure, Table} from "../types";
import {ne} from "../compare";
import {INumeric} from "../types/_numeric";
import {clone} from "../clone";

export interface IDeleteInternalOptions {
  where?: (i: any) => boolean,
  index?: INumeric,
  adjacent?: boolean,
  from?: any,
  to?: any,
}

export function deleteInternal(target: Table, options?: IDeleteInternalOptions): void {
  const result = clone(target);
  result.clear();

  let prev: any = undefined;
  let index = 0;
  for (const i of target.array()) {
    index = index + 1;

    if (options?.where) {
      const row = i instanceof Structure ? i.get() : {table_line: i};
      if (options.where(row) === false) {
        result.append(i, false);
      }
    } else if (options?.adjacent === true && (prev === undefined || ne(prev, i))) {
      result.append(i, false);
    } else if (options?.index && options.index.get() !== index) {
      result.append(i, false);
    }

    prev = i;
  }

  target.clear();
  for (const r of result.array()) {
    target.append(r, false);
  }
}