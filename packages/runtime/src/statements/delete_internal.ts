import {Table} from "../types";
import {ne} from "../compare";

export interface IDeleteInternalOptions {
  where?: (i: any) => boolean,
  adjacent: boolean,
}

export function deleteInternal(target: Table, options?: IDeleteInternalOptions): void {
  const result = new Table();

  let prev: any = undefined;
  for (const i of target.array()) {
    // todo, if i is a structure, then expose structure fields
    const row = {table_line: i};

    if (options) {
      if (options.where && options.where(row) === false) {
        result.append(i);
      } else if (options.adjacent === true && (prev === undefined || ne(prev, i))) {
        result.append(i);
      }
    }

    prev = i;
  }

  target.clear();
  for (const r of result.array()) {
    target.append(r);
  }
}