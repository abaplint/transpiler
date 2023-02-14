import {binarySearchFromRow} from "../binary_search";
import {eq} from "../compare";
import {DataReference, FieldSymbol, Structure, Table} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export interface IReadTableOptions {
  index?: INumeric | number,
  withKey?: (i: any) => boolean,
  into?: INumeric | ICharacter | Structure | Table | DataReference,
  from?: INumeric | ICharacter | Structure | Table | DataReference,
  referenceInto?: DataReference,
  assigning?: FieldSymbol,
  binarySearch?: boolean,
  withKeyValue?: {key: (i: any) => any, value: any}[],
}

export type ReadTableReturn = {
  subrc: number;
  foundIndex: number;
};

function searchWithKey(arr: any, withKey: (i: any) => boolean, startIndex = 0) {
  const isStructured = arr[0] instanceof Structure;
  for (let index = startIndex; index < arr.length; index++) {
    const a = arr[index];
    const row = isStructured ? {table_line: a, ...a.get()} : {table_line: a};
    if (withKey(row) === true) {
      return {
        found: a,
        foundIndex: index + 1,
      };
    }
  }
  return {
    found: undefined,
    foundIndex: 0,
  };
}

/////////////////

export function readTable(table: Table | FieldSymbol, options?: IReadTableOptions): ReadTableReturn {
  let found: any = undefined;
  let foundIndex = 0;

  const arr = table.array();

  if (options?.index) {
    let index = options.index;
    if (typeof index !== "number") {
      index = index.get();
    }

    found = arr[index - 1];
    if (found) {
      foundIndex = index;
    }
  } else if (options?.binarySearch === true
      && options.withKeyValue
      && options.withKey) {
// note: it currently only uses the first key field for binary search, todo

    const first = options.withKeyValue[0];
    const startIndex = binarySearchFromRow(arr, 0, arr.length, first.key, first.value) - 1;

    const searchResult = searchWithKey(arr, options.withKey, startIndex);
    found = searchResult.found;
    foundIndex = searchResult.foundIndex;
  } else if (options?.withKey) {
    const searchResult = searchWithKey(arr, options.withKey);
    found = searchResult.found;
    foundIndex = searchResult.foundIndex;
  } else if (options?.from) {
    if (options.from instanceof FieldSymbol) {
      options.from = options.from.getPointer();
    }
    if (table instanceof FieldSymbol) {
      table = table.getPointer();
    }
    if (table instanceof Table && options.from instanceof Structure) {
      const keys = table.getOptions()?.primaryKey?.keyFields;
      const isStructured = arr[0] instanceof Structure;
      if (keys !== undefined && isStructured === true) {
//        console.dir(keys);
//        console.dir(options.from.get()[keys[0].toLowerCase()]);
        for (const a of arr) {
          foundIndex++;
          let matches = true;
          for (const k of keys) {
            if (eq(a.get()[k.toLowerCase()], options.from.get()[k.toLowerCase()]) === false) {
              matches = false;
              break;
            }
          }
          if (matches === true) {
            found = a;
            break;
          }
        }
      }
    }
    if (found === undefined) {
      foundIndex = 0;
    }
  } else {
    throw new Error("runtime, readTable, unexpected input");
  }

  let subrc = found ? 0 : 4;
  if ((options?.from || options?.binarySearch === true)
      && subrc === 4) {
    subrc = 8;
  }

  // @ts-ignore
  abap.builtin.sy.get().subrc.set(subrc);
  // @ts-ignore
  abap.builtin.sy.get().tabix.set(foundIndex);

  if (options.into && found) {
    if (options.into instanceof DataReference && found instanceof DataReference) {
      options.into.assign(found.getPointer());
    } else if (options.into instanceof DataReference) {
      options.into.assign(found);
    } else {
      options.into.set(found);
    }
  } else if (options.referenceInto && found) {
    options.referenceInto.assign(found);
  } else if (options.assigning && found) {
    options.assigning.assign(found);
  }

  return {subrc, foundIndex};
}