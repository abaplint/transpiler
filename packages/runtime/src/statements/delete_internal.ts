import {Table} from "../types";

export function deleteInternal(target: Table, where?: (i: any) => boolean): void {
  const result = new Table();

  for (const i of target.array()) {
    const row = {table_line: i};
    // todo, if i is a structure, then expose structure fields
    if (where && where(row) === false) {
      result.append(i);
    }
  }

  target.clear();
  for (const r of result.array()) {
    target.append(r);
  }
}