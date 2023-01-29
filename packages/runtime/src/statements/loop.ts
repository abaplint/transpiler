import {FieldSymbol, Integer, ITableKey, Structure, Table} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

type topType = {[name: string]: INumeric | ICharacter};

export interface ILoopOptions {
  where?: (i: any) => Promise<boolean>,
  usingKey?: string,
  from?: Integer,
  to?: Integer,
  topEquals?: topType,
}

function determineFromTo(array: readonly any[], topEquals: topType | undefined, key: ITableKey): { from: any; to: any; } {
  if (topEquals === undefined) {
    // if there is no WHERE supplied, its using the sorting of the secondary key
    return {from: 0, to: array.length};
  }

  // todo
  console.dir(key);

  return {from: 0, to: array.length};
}

export async function* loop(table: Table | FieldSymbol | undefined, options?: ILoopOptions): AsyncGenerator<any, void, unknown> {
  if (table === undefined) {
    throw new Error("LOOP at undefined");
  } else if (table instanceof FieldSymbol) {
    // @ts-ignore
    yield* loop(table.getPointer(), options);
    return;
  }

  const length = table.array().length;
  if (length === 0) {
    // @ts-ignore
    abap.builtin.sy.get().subrc.set(4);
    return;
  }

  let loopFrom = options?.from && options?.from.get() > 0 ? options.from.get() - 1 : 0;
  let loopTo = options?.to && options.to.get() < length ? options.to.get() : length;
  const loopIndex = table.startLoop(loopFrom);
  let entered = false;

  let array = table.array();

  if (options?.usingKey && options.usingKey !== undefined && options.usingKey !== "primary_key") {
    array = table.getSecondaryIndex(options.usingKey);

    const {from, to} = determineFromTo(array, options.topEquals, table.getKeyByName(options.usingKey)!);
    loopFrom = Math.max(loopFrom, from);
    loopTo = Math.min(loopTo, to);
  }

  try {
    const isStructured = array[0] instanceof Structure;

    while (loopIndex.index < loopTo) {
      if (loopIndex.index > array.length) {
        break;
      }
      const current = array[loopIndex.index];

      if (options?.where) {
        const row = isStructured ? current.get() : {table_line: current};
        if (await options.where(row) === false) {
          loopIndex.index++;
          continue;
        }
      }

      // @ts-ignore
      abap.builtin.sy.get().tabix.set(loopIndex.index + 1);
      entered = true;

      yield current;

      loopIndex.index++;
      loopTo = options?.to && options.to.get() < array.length ? options.to.get() : array.length;
    }
  } finally {
    table.unregisterLoop(loopIndex);
    // @ts-ignore
    abap.builtin.sy.get().subrc.set(entered ? 0 : 4);
  }
}
