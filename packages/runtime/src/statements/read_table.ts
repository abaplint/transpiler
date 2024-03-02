import {binarySearchFrom, binarySearchFromRow} from "../binary_search";
import {eq, ge, gt} from "../compare";
import {DataReference, DecFloat34, FieldSymbol, Float, HashedTable, Integer8, Structure, Table, TableAccessType} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export interface IReadTableOptions {
  index?: INumeric | Integer8 | FieldSymbol | number,
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

/** startIndex = javascript index, return ABAP index */
function searchWithKeyEarlyExit(arr: any,
                                withKey: (i: any) => boolean,
                                startIndex = 0,
                                usesTableLine: boolean | undefined,
                                firstKeyName: string,
                                firstValue: any) {
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
    /*
    console.dir(row);
    console.dir(row[firstKeyName.toLowerCase()]);
    console.dir(firstValue);
    */
    if (gt(row[firstKeyName.toLowerCase()], firstValue)) {
      return {
        found: undefined,
        foundIndex: 0,
      };
    }
  }
  return {
    found: undefined,
    foundIndex: 0,
  };
}

/** startIndex = javascript index, return ABAP index */
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
  let binarySubrc: number | undefined = undefined;

//  console.dir(options);

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

    if (table instanceof HashedTable) {
      // hashed tables requires all fields for fast lookup
      const fields = new Set<string>(table.getOptions().primaryKey!.keyFields);
      for (const name in options.withKeySimple) {
        fields.delete(name.toUpperCase());
      }
      if (fields.size === 0) {
        options.withTableKey = true;
      }
    } else {
      // while sorted just needs the first key field
      const firstKeyField = table.getOptions().primaryKey!.keyFields[0];
      let useKey = false;
      for (const name in options.withKeySimple) {
        if (firstKeyField === name.toUpperCase()) {
          useKey = true;
        }
      }
      if (useKey === true) {
        options.withTableKey = true;
      }
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
      } else if (index instanceof Integer8) {
        index = Number(index.get());
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
  } else if (options?.keyName && options.withKey && options.withKeySimple) {
    const arr = table.getSecondaryIndex(options.keyName);
    const keyInformation = table.getKeyByName(options.keyName);
    const firstKeyName = keyInformation?.keyFields[0];
    if (firstKeyName === undefined) {
      throw new Error("readTable, first key name not found");
    }

//    console.dir("SEC:");
    const firstValue = options.withKeySimple[firstKeyName.toLowerCase()];
    if (firstValue === undefined) {
      // fallback
      return readTable(table, {...options, withKeySimple: undefined});
    }

    const startIndex = binarySearchFrom(arr, 0, arr.length, firstKeyName.toLowerCase(), firstValue) - 1;
//    console.dir("startindex: " + startIndex);

    if (startIndex >= 0) {
      const searchResult = searchWithKeyEarlyExit(arr, options.withKey, startIndex, options.usesTableLine, firstKeyName, firstValue);
//      console.dir(searchResult);

      found = searchResult.found;
      foundIndex = searchResult.foundIndex;
    }

  } else if ((options?.binarySearch === true || options?.withTableKey === true)
      && options.withKeyValue
      && ( options?.binarySearch === true || table.getOptions().primaryKey?.type !== TableAccessType.standard )
      && options.withKey) {
// note: it currently only uses the first key field for binary search, todo
    const first = options.withKeyValue[0];
    const arr = table.array();
    const startIndex = binarySearchFromRow(arr, 0, arr.length - 1, first.key, first.value, options.usesTableLine);

    // todo: early exit if not found
    const searchResult = searchWithKey(arr, options.withKey, startIndex, options.usesTableLine);
    found = searchResult.found;
    foundIndex = searchResult.foundIndex;

    if (found === undefined) {
      if (arr.length === 0) {
        binarySubrc = 8;
        foundIndex = 1;
      } else {
        binarySubrc = 4;
        foundIndex = startIndex + 1;

        // check if going beyond the last row, todo: only checks one field
        const last = arr[arr.length - 1];
        const isStructured = last instanceof Structure;
        let row: any = undefined;
        if (options.usesTableLine === false && isStructured === true) {
          row = last.get();
        } else {
          row = isStructured ? {table_line: last, ...last.get()} : {table_line: last};
        }
        if (ge(first.value, first.key(row))) {
          binarySubrc = 8;
        }
      }
    }
  } else if (options?.withKey) {
    const arr = table.array();
    const searchResult = searchWithKey(arr, options.withKey, 0, options.usesTableLine);
    found = searchResult.found;
    foundIndex = searchResult.foundIndex;
  } else if (options?.from) {
    if (options.from instanceof FieldSymbol) {
      options.from = options.from.getPointer();
    }

    if (table instanceof Table && options.from instanceof Structure) {
      // todo: optimize if the primary key is sorted
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
    } else if (table instanceof HashedTable && options.from instanceof Structure) {
      const key = table.buildHashFromData(options.from);
      found = table.read(key);
    }

    if (found === undefined) {
      foundIndex = 0;
    }
    if (found === undefined && table.getOptions().primaryKey?.type === TableAccessType.sorted) {
      binarySubrc = 8;
    }
  } else {
    throw new Error("runtime, readTable, unexpected input");
  }

  let subrc = found ? 0 : 4;
  if (binarySubrc) {
    subrc = binarySubrc;
  } else if ((options?.binarySearch === true || options?.keyName !== undefined)
      && subrc === 4) {
    subrc = 8;
  }

  // @ts-ignore
  abap.builtin.sy.get().subrc.set(subrc);
  // @ts-ignore
  abap.builtin.sy.get().tabix.set(foundIndex);

  if (found) {
    if (options.into) {
      if (options.into instanceof DataReference) {
        if (found instanceof DataReference) {
          options.into.assign(found.getPointer());
        } else {
          options.into.assign(found);
        }
      } else {
        options.into.set(found);
      }
    } else if (options.referenceInto) {
      options.referenceInto.assign(found);
    } else if (options.assigning) {
      options.assigning.assign(found);
    }
  }

  return {subrc, foundIndex};
}