import {Character, Date, FieldSymbol, HashedTable, Hex, HexUInt8, Numc, String, Structure, Table, TableKeyType, Time, XString} from "../types";
import {eq} from "../compare";
import {INumeric} from "../types/_numeric";
import {loop} from "./loop";
import {ABAP} from "..";

declare const abap: ABAP;

export interface IDeleteInternalOptions {
  where?: (i: any) => Promise<boolean>,
  index?: INumeric,
  adjacent?: boolean,
  comparing?: string[],
  allFields?: boolean,
  fromValue?: any,
  from?: any,
  to?: any,
}

// the standard key, all character-like and byte-like components, substructures expanded
function standardKeyValues(row: any): any[] {
  if (!(row instanceof Structure)) {
    return [row];
  }
  const ret: any[] = [];
  for (const component of Object.values(row.get())) {
    if (component instanceof Structure) {
      ret.push(...standardKeyValues(component));
    } else if (component instanceof Character
        || component instanceof Numc
        || component instanceof Date
        || component instanceof Time
        || component instanceof String
        || component instanceof Hex
        || component instanceof HexUInt8
        || component instanceof XString) {
      ret.push(component);
    }
  }
  return ret;
}

// values of the primary key, an empty key gives no values
function primaryKeyValues(target: Table, row: any): any[] {
  const options = target.getOptions();
  if (options?.keyType === TableKeyType.empty) {
    return [];
  }
  const keyFields = options?.primaryKey?.keyFields ?? [];
  if (keyFields.length === 0) {
    return standardKeyValues(row);
  }
  return keyFields.map(k => {
    if (k.toUpperCase() === "TABLE_LINE") {
      return row;
    }
    let value = row;
    for (const name of k.toLowerCase().split("-")) {
      value = value.get()[name];
    }
    return value;
  });
}

export async function deleteInternal(target: Table | HashedTable | FieldSymbol, options?: IDeleteInternalOptions): Promise<void> {
  let index = 0;

  if (target instanceof FieldSymbol) {
    target = target.getPointer() as Table;
    if (target === undefined) {
      throw new Error("deleteInternal, FS not assigned");
    }
  }

  if (options?.index
      && options?.where === undefined
      && options?.adjacent === undefined
      && options?.fromValue === undefined
      && options?.from === undefined
      && options?.to === undefined) {
    if (options.index.get() === 0) {
      throw new Error("TABLE_INVALID_INDEX");
    }

    if (target.array()[options.index.get() - 1] === undefined) {
      abap.builtin.sy.get().subrc.set(4);
      return;
    } else {
      target.deleteIndex(options.index.get() - 1);
      abap.builtin.sy.get().subrc.set(0);
      return;
    }
  }

  if (options?.to) {
    if (options?.where !== undefined) {
      throw new Error("DeleteInternalTodo");
    }
    const from = options.from?.get() ?? 1;
    const to = Math.min(options.to.get(), target.array().length);
    for (let i = to; i >= from; i--) {
      target.deleteIndex(i - 1);
    }
    return;
  }

  if (options?.adjacent === true) {
    if (target instanceof HashedTable) {
      throw new Error("delete adjacent, hashed table");
    }

    const array = target.array();

    for (let index = array.length - 1; index > 0; index--) {

      const prev = array[ index - 1];
      const i = array[ index ];

      if (options?.allFields === true) {
        if (eq(prev, i) === true) {
          target.deleteIndex(index);
        }
      } else if (options?.comparing) {
        let match = false;
        for (const compareField of options.comparing) {
          match = eq(prev.get()[compareField], i.get()[compareField]);
          if (!match) {
            break;
          }
        }
        if (match) {
          target.deleteIndex(index);
        }
      } else {
        // without COMPARING, rows are compared by the primary key, nothing is deleted if the key is empty
        const prevKey = primaryKeyValues(target, prev);
        const key = primaryKeyValues(target, i);
        if (key.length > 0 && key.every((value, n) => eq(prevKey[n], value))) {
          target.deleteIndex(index);
        }
      }
    }
    return;
  }

  if (target instanceof HashedTable && options?.fromValue) {
    target.deleteFrom(options.fromValue);
    return;
  }

  // short form, "DELETE tab"
  if (options === undefined) {
    target.deleteIndex((target as Table).getCurrentLoopIndex());
    return;
  }

  let deleted = 0;
  for await (const i of loop(target)) {
    index = abap.builtin.sy.get().tabix.get() - 1;

    if (options?.where) {
      const row = i instanceof Structure ? i.get() : {table_line: i};
      if (await options.where(row) === true) {
        if (target instanceof HashedTable) {
          target.deleteFrom(i);
        } else {
          target.deleteIndex(index);
        }
        deleted++;
      }
    } else if (options?.index && options.index.get() === index) {
      target.deleteIndex(options.index.get() - 1);
      break;
    } else if (options?.fromValue && eq(options.fromValue, i)) {
      target.deleteIndex(index);
    } else if (options?.from && options.from.get() <= index + 1) {
      target.deleteIndex(index);
    }
  }

  if (options?.where) {
    abap.builtin.sy.get().subrc.set(deleted > 0 ? 0 : 4);
  }
}