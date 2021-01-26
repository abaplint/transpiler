import {Integer,Structure, Table} from "../types";

export interface ILoopOptions {
  where?: (i: any) => boolean,
  from?: Integer,
  to?: Integer
}

export function* loop(table: Table, options?: ILoopOptions) {
  const length = table.array().length;
  if (length === 0) {
    return;
  }

  const loopFrom = options?.from && options?.from.get() > 0 ? options.from.get() - 1 : 0;
  let loopTo = options?.to && options.to.get() < length ? options.to.get() : length;
  const loopIndex = table.startLoop(loopFrom);

  try {
    const array = table.array();
    const isStructured = array[0] instanceof Structure;

    while (loopIndex.index < loopTo) {
      if (loopIndex.index > array.length) {
        break;
      }
      const current = array[loopIndex.index];

      if (options?.where) {
        const row = isStructured ? current.get() : {table_line: current};
        if (options.where(row) === false) {
          loopIndex.index++;
          continue;
        }
      }

      // @ts-ignore
      abap.builtin.sy.get().tabix.set(loopIndex.index + 1);

      yield current;

      loopIndex.index++;
      loopTo = options?.to && options.to.get() < array.length ? options.to.get() : array.length;
    }
  } finally {
    table.unregisterLoop(loopIndex);
  }

}