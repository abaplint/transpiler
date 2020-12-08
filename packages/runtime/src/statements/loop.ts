import {Structure, Table} from "../types";

export function* loop(
  table: Table,
  where: (i: any) => boolean | undefined) {

  const array = table.array();
  for (let i = 0; i < array.length; i++) {
    // @ts-ignore
    abap.builtin.sy.get().tabix.set(i + 1);

    if (where) {
      const row = array[i] instanceof Structure ? array[i].get() : {table_line: array[i]};
      if (where(row) === false) {
        continue;
      }
    }

    yield array[i];
  }
}