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
}

export function readTable(table: Table | FieldSymbol, options?: IReadTableOptions) {
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
  } else if (options?.withKey) {
    const isStructured = arr[0] instanceof Structure;
    for (const a of arr) {
      foundIndex++;
      const row = isStructured ? {table_line: a, ...a.get()} : {table_line: a};
      if (options.withKey(row) === true) {
        found = a;
        break;
      }
    }
    if (found === undefined) {
      foundIndex = 0;
    }
  } else if (options?.from) {
    if (table instanceof Table && options.from instanceof Structure) {
      const keys = table.getOptions()?.keyFields;
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
            found = arr;
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
  if (options?.from && subrc === 4) {
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