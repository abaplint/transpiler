import {Structure, Table} from "../types";

export function* loop(
  table: Table,
  where?: (i: any) => boolean | undefined) {

  let prev: any | undefined = undefined;
  let index = 0;
  while (index < table.array().length) {
    const array = table.array();
    let current = array[index];
    if (prev && current === prev) {
      index++;
      current = array[index];
      if (current === undefined) {
        continue;
      }
    }

    // @ts-ignore
    abap.builtin.sy.get().tabix.set(index + 1);

    if (where) {
      const row = current instanceof Structure ? current.get() : {table_line: current};
      if (where(row) === false) {
        index++;
        continue;
      }
    }

    prev = current;
    yield current;
  }

  /*
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
  */
}