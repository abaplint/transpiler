import {binarySearchFrom, binarySearchTo} from "../binary_search";
import {eq, lt} from "../compare";
import {Character, FieldSymbol, HashedTable, Hex, Integer, ITableKey, Numc, String as AString,
  secondaryKeyName, Structure, Table, TableAccessType} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {parsePosition} from "../operators/_parse";
import {ABAP} from "..";

declare const abap: ABAP;

type topType = {[name: string]: INumeric | ICharacter};

export interface ILoopOptions {
  where?: (i: any) => Promise<boolean>,
  atLast?: (last: boolean) => void,
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

  // 1-based, as the branch without topEquals above answers: when the key's
  // first field is not in topEquals (a key over a substructure component,
  // s-x, is never put there) nothing is narrowed, and the loop must not start
  // at row index -1
  let from = 1;
  let to = array.length;

// todo: multi field
  const keyField = key.keyFields[0].toLowerCase();
  const keyValue = topEquals[keyField];
  if (keyField && keyValue) {
    from = binarySearchFrom(array, 0, to, keyField, keyValue);
    to = binarySearchTo(array, from, to, keyField, keyValue);
//    console.dir("from: " + from + ", to: " + to);
  }

  return {
    from: from,
    to: to,
  };
}

/** -1 / 0 / 1: the key field x sorts before / equal to / after the WHERE operand
 *  v - only for pairs whose order agrees with the table's sort (sort.ts compares
 *  the fields with lt/eq): the same type, of the same length for c, n and x, or
 *  a c operand against a string field, which eq compares by its getTrimEnd() */
function keyComparator(sample: any, value: any): ((x: any, v: any) => number) | undefined {
  if (value === null || typeof value !== "object" || sample === null || typeof sample !== "object") {
    return undefined;
  }
  if (sample.constructor === value.constructor) {
    const fixed = sample instanceof Character || sample instanceof Numc || sample instanceof Hex;
    if (fixed && sample.getLength() !== value.getLength()) {
      return undefined;
    }
    return (x, v) => (eq(x, v) ? 0 : lt(x, v) ? -1 : 1);
  }
  if (sample instanceof AString && value instanceof Character) {
    return (x, v) => {
      const s = v.getTrimEnd();
      const xs = x.get();
      return xs === s ? 0 : xs < s ? -1 : 1;
    };
  }
  return undefined;
}

/** A SORTED primary key, and a WHERE that requires its first field to equal a
 *  value: the rows the WHERE can accept are one block of the array. Relies on
 *  topEquals holding only conditions every row has to meet - see the
 *  transpiler's LoopTranspiler, which emits it for a conjunction only */
function sortedPrimaryBlock(table: Table | HashedTable, options: ILoopOptions | undefined): ((row: any) => number) | undefined {
  const primary = table.getOptions()?.primaryKey;
  if (!(table instanceof Table) || primary?.type !== TableAccessType.sorted
      || !primary.keyFields?.length || options?.topEquals === undefined) {
    return undefined;
  }
  const field = primary.keyFields[0].toLowerCase();
  const value = options.topEquals[field];
  const rowType = table.getRowType();
  const structured = rowType instanceof Structure;
  if (value === undefined || structured === (field === "table_line")) {
    return undefined;
  }
  const compare = keyComparator(structured ? (rowType as Structure).get()[field] : rowType, value);
  if (compare === undefined) {
    return undefined;
  }
  return structured ? (row: any) => compare(row.get()[field], value) : (row: any) => compare(row, value);
}

// todo: rewrite, this is a mess & hack & slow
function dynamicToWhere(condition: string, evaluate: (name: string) => FieldSymbol | undefined): (placeholder: any) => Promise<boolean> {
//  console.dir(condition);
  let text = condition.replace(/ AND /gi, " && ").replace(/ OR /gi, " || ").replace(/ = /gi, " EQ ").replace(/ <> /gi, " NE ");
//  console.dir(text);

  if (evaluate === undefined) {
    throw new Error("Dynamic WHERE evaluation function is not defined");
  }

  const matches = text.matchAll(/([\w-]+)\s+(NOT\s+IN|\w+)\s+([<>\w-]+)/gi);
  for (const match of matches) {
    const left = match[1];
    const comparator = match[2].toLowerCase().replace(/\s+/g, " ");
    let right = "";
//    console.dir({left, right});

    const cleft = "i." + left.toLowerCase().replace(/-/g, ".get().");

    const rightMatches = match[3].matchAll(/<(\w+)>-(\w+)/gi);
    for (const rightMatch of rightMatches) {
      const name = "fs_" + rightMatch[1].toLowerCase() + "_";
      right = `evaluate("${name}").get()["${rightMatch[2].toLowerCase()}"]`;
    }

    const fieldSymbol = match[3].match(/^<(\w+)>$/);
    if (right === "" && fieldSymbol) {
      right = `evaluate("fs_${fieldSymbol[1].toLowerCase()}_").getPointer()`;
    }

    if (right === "") {
      right = `evaluate("${match[3].toLowerCase()}")`;
    }

    if (comparator === "in") {
      const cnew = `abap.compare.in(${cleft}, ${right})`;
      text = text.replace(match[0], cnew);
    } else if (comparator === "not in") {
      const cnew = `!abap.compare.in(${cleft}, ${right})`;
      text = text.replace(match[0], cnew);
    } else {
      const cnew = `abap.compare.${comparator}(${cleft}, ${right})`;
      text = text.replace(match[0], cnew);
    }
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
    abap.builtin.sy.get().subrc.set(4);
    return;
  }

  // FROM and TO are positions of type i, an arithmetic result outside it raises
  const optionFrom = options?.from ? parsePosition(options.from) : undefined;
  const optionTo = options?.to ? parsePosition(options.to) : undefined;
  // checked against undefined, a bound of 0 is a bound: TO 0 runs no row
  let loopFrom = optionFrom !== undefined && optionFrom > 0 ? optionFrom - 1 : 0;
  let loopTo = optionTo !== undefined && optionTo < length ? optionTo : length;

  let array: any[] = [];
  let block: ((row: any) => number) | undefined = undefined;
  // the secondary key the loop runs over; undefined is the primary key, also
  // when it is named - USING KEY PRIMARY_KEY, or a dynamic name holding it
  const usingKey = secondaryKeyName(options?.usingKey);
  if (usingKey !== undefined) {
    array = table.getSecondaryIndex(usingKey);

    const {from, to} = determineFromTo(array, options?.topEquals, table.getKeyByName(usingKey)!);
    loopFrom = Math.max(loopFrom, from) - 1;
    loopTo = Math.min(loopTo, to);
  } else {
    array = table.array();
    if (options?.where !== undefined && options.from === undefined && options.to === undefined) {
      block = sortedPrimaryBlock(table, options);
    }
    if (block !== undefined) {
      // the first row that does not sort before the value
      let lo = 0;
      let hi = array.length;
      while (lo < hi) {
        const mid = Math.floor((lo + hi) / 2);
        if (block(array[mid]) < 0) {
          lo = mid + 1;
        } else {
          hi = mid;
        }
      }
      loopFrom = lo;
    }
  }

  const loopController = table.startLoop(loopFrom, loopTo, array);
  let entered = false;

  // ABAP hands sy-tabix back the way it found it. A LOOP owns sy-tabix only
  // for as long as it runs; leaving it, by ENDLOOP or EXIT or RETURN or an
  // exception, puts back whatever the enclosing loop had. Without that an
  // inner loop, or a method that happens to contain one, silently rewrites
  // the row number the outer loop is standing on, and the outer body reads
  // the inner loop's last index as its own.
  const outerTabix = abap.builtin.sy.get().tabix.get();

  // A hashed table has no row number to report, and ABAP says so by leaving
  // sy-tabix at 0 for the whole loop rather than by inventing a position.
  // The same goes for a loop that reads an index table through a hash
  // secondary key. Handing out 1, 2, 3 there looks helpful and is a lie the
  // caller cannot tell from the truth.
  const usedKey = usingKey === undefined ? undefined : table.getKeyByName(usingKey);
  const hasRowNumber = usedKey !== undefined
    ? usedKey.type !== TableAccessType.hashed
    : !(table instanceof HashedTable);

  try {
    const isStructured = array[0] instanceof Structure;

    while (loopController.index < loopController.loopTo) {
      // the body may have deleted rows: never read past the end, where
      // array[array.length] is undefined
      if (loopController.index >= array.length) {
        break;
      }
      const current = array[loopController.index];

      if (block !== undefined && block(current) > 0) {
        // this row sorts after the value, and so does every row behind it
        break;
      }

      if (options?.where) {
        const row = isStructured ? current.get() : {table_line: current};
        if (await options.where(row) === false) {
          loopController.index++;
          continue;
        }
      }

      abap.builtin.sy.get().tabix.set(hasRowNumber ? loopController.index + 1 : 0);
      entered = true;

      options?.atLast?.(loopController.index + 1 >= loopController.loopTo);

      yield current;

      loopController.index++;

      if (options?.to === undefined && usingKey === undefined) {
        // extra rows might have been inserted inside the loop
        loopController.loopTo = array.length;
      }
    }
  } finally {
    table.unregisterLoop(loopController);
    abap.builtin.sy.get().subrc.set(entered ? 0 : 4);
    abap.builtin.sy.get().tabix.set(outerTabix);
  }
}