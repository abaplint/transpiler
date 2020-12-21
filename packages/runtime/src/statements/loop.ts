import {Structure, Table} from "../types";

export function* loop(
  table: Table,
  where?: (i: any) => boolean) {

  const loopIndex = table.startLoop();

  try {
    while (loopIndex.index < table.array().length) {
      const array = table.array();
      const current = array[loopIndex.index];
//      console.dir("index: " + loopIndex.index);

      // @ts-ignore
      abap.builtin.sy.get().tabix.set(loopIndex.index + 1);

      if (where) {
        const row = current instanceof Structure ? current.get() : {table_line: current};
        if (where(row) === false) {
          loopIndex.index++;
          continue;
        }
      }

      yield current;

      loopIndex.index++;
//      console.dir("incremeted " + loopIndex.index);
    }
  } finally {
    table.unregisterLoop(loopIndex);
  }

}