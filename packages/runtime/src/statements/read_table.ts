import {binarySearchFromRow} from "../binary_search.js";
import {eq} from "../compare/index.js";
import {DataReference, DecFloat34, FieldSymbol, Float, HashedTable, Structure, Table, TableAccessType} from "../types/index.js";
import {ICharacter} from "../types/_character.js";
import {INumeric} from "../types/_numeric.js";

export interface IReadTableOptions {
  index?: INumeric | FieldSymbol | number,
  into?: INumeric | ICharacter | Structure | Table | DataReference,
  from?: INumeric | ICharacter | Structure | Table | DataReference,
  referenceInto?: DataReference,
  assigning?: FieldSymbol,
  binarySearch?: boolean,
  withTableKey?: boolean,
  keyName?: string,
  usesTableLine?: boolean,
  // single function, evaluates full condition
  withKey?: (i: any) => boolean,
  // used for binary search, one function per field
  withKeyValue?: {key: (i: any) => any, value: any}[],
  // key is raw concatenated, not eval'uable
  withKeySimple?: {[key: string]: any},
// TODO: TRANSPORTING
}

export type ReadTableReturn = {
  subrc: number;
  foundIndex: number;
};

function searchWithKey(arr: any, withKey: (i: any) => boolean, startIndex = 0, usesTableLine: boolean | undefined) {
  const isStructured = arr[0] instanceof Structure;
  for (let index = startIndex; index < arr.length; index++) {
    const a = arr[index];
    let row: any = undefined;
    if (usesTableLine === false && isStructured === true) {
      row = a.get();
    } else {
      row = isStructured ? {table_line: a, ...a.get()} : {table_line: a};
    }
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

export function readTable(table: Table | HashedTable | FieldSymbol, options?: IReadTableOptions): ReadTableReturn {
  let found: any = undefined;
  let foundIndex = 0;

  if (table instanceof FieldSymbol) {
    if (table.getPointer() === undefined) {
      throw new Error("GETWA_NOT_ASSIGNED");
    }
    return readTable(table.getPointer(), options);
  }

  // check if it is a primary index read specified with WITH KEY instead of WITH TABLE KEY
  if (options?.withTableKey === undefined
      && options?.withKeySimple
      && (table.getOptions().primaryKey?.keyFields || []).length > 0) {
    const fields = new Set<string>(table.getOptions().primaryKey!.keyFields);
    for (const name in options.withKeySimple) {
      fields.delete(name.toUpperCase());
    }
    if (fields.size === 0) {
      options.withTableKey = true;
    }
  }

  if (options?.index) {
    if (table instanceof HashedTable) {
      throw new Error("Hashed table, READ INDEX not possible");
    }
    const arr = table.array();
    let index = options.index;
    if (typeof index !== "number") {
      if (index instanceof FieldSymbol) {
        if (index.getPointer() === undefined) {
          throw new Error("GETWA_NOT_ASSIGNED");
        }
        index = index.getPointer() as INumeric;
      }

      if (index instanceof Float || index instanceof DecFloat34) {
        index = index.getRaw();
      } else {
        index = index.get();
      }
    }

    found = arr[index - 1];
    if (found) {
      foundIndex = index;
    }
  } else if (table instanceof HashedTable && options?.withTableKey === true && options.withKeySimple) {
    const hash = table.buildHashFromSimple(options.withKeySimple);
    found = table.read(hash);
    foundIndex = 0;
  } else if (table instanceof HashedTable && options?.withKey) {
    // this is slow..
    const searchResult = searchWithKey(table.array(), options.withKey, 0, options?.usesTableLine);
    found = searchResult.found;
    foundIndex = 0;
  } else if ((options?.binarySearch === true || options?.withTableKey === true)
      && options.withKeyValue
      && ( options?.binarySearch === true || table.getOptions().primaryKey?.type !== TableAccessType.standard )
      && options.withKey) {
// note: it currently only uses the first key field for binary search, todo
    const first = options.withKeyValue[0];
    const arr = table.array();
    const startIndex = binarySearchFromRow(arr, 0, arr.length, first.key, first.value, options.usesTableLine) - 1;

    const searchResult = searchWithKey(arr, options.withKey, startIndex, options.usesTableLine);
    found = searchResult.found;
    foundIndex = searchResult.foundIndex;
  } else if (options?.withKey) {
    const arr = table.array();
    const searchResult = searchWithKey(arr, options.withKey, 0, options.usesTableLine);
    found = searchResult.found;
    foundIndex = searchResult.foundIndex;
  } else if (options?.from) {
    if (options.from instanceof FieldSymbol) {
      options.from = options.from.getPointer();
    }
    if (table instanceof HashedTable) {
      throw new Error("runtime, todo readTable Hashed FROM");
    }
    if (table instanceof Table && options.from instanceof Structure) {
      const arr = table.array();
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
  if ((options?.from || options?.binarySearch === true || options?.keyName !== undefined)
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