import {Table} from "../types";
import {ne} from "../compare";
import {INumeric} from "../types/_numeric";
import {clone} from "../clone";

export interface IDeleteInternalOptions {
  where?: (i: any) => boolean,
  index: INumeric,
  adjacent: boolean,
}

export function deleteInternal(target: Table, options?: IDeleteInternalOptions): void {
  const result = clone(target);
  result.clear();

  let prev: any = undefined;
  let index = 0;
  for (const i of target.array()) {
    index = index + 1;
    // todo, if i is a structure, then expose structure fields
    const row = {table_line: i};

    if (options?.where && options.where(row) === false) {
      result.append(i);
    } else if (options?.adjacent === true && (prev === undefined || ne(prev, i))) {
      result.append(i);
    } else if (options?.index && options.index.get() !== index) {
      result.append(i);
    }

    prev = i;
  }

  target.clear();
  for (const r of result.array()) {
    target.append(r);
  }
}