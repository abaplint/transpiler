import {binarySearchFrom, binarySearchTo} from "../binary_search";
import {FieldSymbol, HashedTable, Integer, ITableKey, Structure, Table} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

type topType = {[name: string]: INumeric | ICharacter};

export interface ILoopOptions {
  where?: (i: any) => Promise<boolean>,
  usingKey?: string,
  from?: Integer,
  to?: Integer,
  topEquals?: topType,
  dynamicWhere?: {condition: string, evaluate: (name: string) => FieldSymbol | undefined},
}

function determineFromTo(array: readonly any[], topEquals: topType | undefined, key: ITableKey): { from: any; to: any; } {
  if (topEquals === undefined) {
    // if there is no WHERE supplied, its using the sorting of the secondary key
    return {from: 1, to: array.length};
  }

  let from = 0;
  let to = array.length;

// todo: multi field
  const keyField = key.keyFields[0].toLowerCase();
  const keyValue = topEquals[keyField];
  if (keyField && keyValue) {
    from = binarySearchFrom(array, from, to, keyField, keyValue);
    to = binarySearchTo(array, from, to, keyField, keyValue);
//    console.dir("from: " + from + ", to: " + to);
  }

  return {
    from: from,
    to: to,
  };
}

// todo: rewrite, this is a mess & hack & slow
function dynamicToWhere(condition: string, evaluate: (name: string) => FieldSymbol | undefined): (placeholder: any) => Promise<boolean> {
//  console.dir(condition);
  let text = condition.replace(/ AND /gi, " && ").replace(/ OR /gi, " || ").replace(/ = /gi, " EQ ").replace(/ <> /gi, " NE ")
//  console.dir(text);

  let regex = /([\w-]+)\s+(\w+)\s+([<>\w-]+)/gi;
  let matches = text.matchAll(regex);
  for (const match of matches) {
    const left = match[1];
    const comparator = match[2].toLowerCase();
    const right = match[3];
//    console.dir({left, right});

    const cleft = "i." + left.toLowerCase().replace(/-/g, ".get().");
    const cright = right;
    const cnew = `abap.compare.${comparator}(${cleft}, ${cright})`;
    text = text.replace(match[0], cnew);
  }

  regex = / <(\w+)>-(\w+)/gi;
  matches = text.matchAll(regex);
  for (const match of matches) {
    const name = "fs_" + match[1].toLowerCase() + "_";
    const value = evaluate(name)?.get()?.[match[2].toLowerCase()]?.get();
    text = text.replace(match[0], " '" + value + "'");
  }

//  console.dir(text);

  // @ts-ignore
  return async (i: any) => {
//    console.dir(i);
    // eslint-disable-next-line no-eval
    return eval(text);
  };
}

export async function* loop(table: Table | HashedTable | FieldSymbol | undefined,
                            options?: ILoopOptions): AsyncGenerator<any, void, unknown> {

  if (table === undefined) {
    throw new Error("LOOP at undefined");
  } else if (table instanceof FieldSymbol) {
    const pnt = table.getPointer();
    if (pnt === undefined) {
      throw new Error("GETWA_NOT_ASSIGNED");
    }
    yield* loop(pnt, options);
    return;
  }

  if (options?.dynamicWhere) {
    const dynamicWhere = options.dynamicWhere;
    const newOptions = {...options};
    delete newOptions.dynamicWhere;
    newOptions.where = dynamicToWhere(dynamicWhere.condition, dynamicWhere.evaluate);
    yield* loop(table, newOptions);
    return;
  }

  const length = table.getArrayLength();
  if (length === 0) {
    // @ts-ignore
    abap.builtin.sy.get().subrc.set(4);
    return;
  }

  let loopFrom = options?.from && options?.from.get() > 0 ? options.from.get() - 1 : 0;
  let loopTo = options?.to && options.to.get() < length ? options.to.get() : length;

  let array: any[] = [];
  if (options?.usingKey && options.usingKey !== undefined && options.usingKey !== "primary_key") {
    array = table.getSecondaryIndex(options.usingKey);

    const {from, to} = determineFromTo(array, options.topEquals, table.getKeyByName(options.usingKey)!);
    loopFrom = Math.max(loopFrom, from) - 1;
    loopTo = Math.min(loopTo, to);
  } else {
    array = table.array();
  }

  const loopController = table.startLoop(loopFrom, loopTo, array);
  let entered = false;

  try {
    const isStructured = array[0] instanceof Structure;

    while (loopController.index < loopController.loopTo) {
      if (loopController.index > array.length) {
        break;
      }
      const current = array[loopController.index];

      if (options?.where) {
        const row = isStructured ? current.get() : {table_line: current};
        if (await options.where(row) === false) {
          loopController.index++;
          continue;
        }
      }

      // @ts-ignore
      abap.builtin.sy.get().tabix.set(loopController.index + 1);
      entered = true;

      yield current;

      loopController.index++;

      if (options?.to === undefined && options?.usingKey === undefined) {
        // extra rows might have been inserted inside the loop
        loopController.loopTo = array.length;
      }
    }
  } finally {
    table.unregisterLoop(loopController);
    // @ts-ignore
    abap.builtin.sy.get().subrc.set(entered ? 0 : 4);
  }
}
