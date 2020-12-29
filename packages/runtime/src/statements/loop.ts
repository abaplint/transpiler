import {Integer,Structure, Table} from "../types";

export interface ILoopOptions {
  where?: (i: any) => boolean,
  from?: Integer,
  to?: Integer
}

export function* loop(table: Table, options?: ILoopOptions) {
  const loopFrom = options?.from && options.from.get() > 0 ? options.from.get() - 1 : 0;
  let loopTo = options?.to && options.to.get() < table.array().length ? options.to.get() : table.array().length;
  const loopIndex = table.startLoop(loopFrom);

  try {
    while (loopIndex.index < loopTo) {
      if (loopIndex.index > table.array().length) {
        break;
      }
      const array = table.array();
      const current = array[loopIndex.index];

      // @ts-ignore
      abap.builtin.sy.get().tabix.set(loopIndex.index + 1);

      if (options?.where) {
        const row = current instanceof Structure ? current.get() : {table_line: current};
        if (options.where(row) === false) {
          loopIndex.index++;
          continue;
        }
      }

      yield current;

      loopIndex.index++;
      loopTo = options?.to && options.to.get() < table.array().length ? options.to.get() : table.array().length;
    }
  } finally {
    table.unregisterLoop(loopIndex);
  }

}