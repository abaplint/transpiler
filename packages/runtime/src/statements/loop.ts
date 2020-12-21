import {Structure, Table} from "../types";

export function* loop(
  table: Table,
  where?: (i: any) => boolean | undefined) {

  const loopIndex = table.startLoop();

  let prev: any | undefined = undefined;
  while (loopIndex.index < table.array().length) {
    const array = table.array();
    let current = array[loopIndex.index];
    if (prev && current === prev) {
      loopIndex.index++;
      current = array[loopIndex.index];
      if (current === undefined) {
        continue;
      }
    }

    // @ts-ignore
    abap.builtin.sy.get().tabix.set(loopIndex.index + 1);

    if (where) {
      const row = current instanceof Structure ? current.get() : {table_line: current};
      if (where(row) === false) {
        loopIndex.index++;
        continue;
      }
    }

    prev = current;
    try {
      yield current;
    } finally {
      table.unregisterLoop(loopIndex);
    }
  }

}