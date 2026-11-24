import {ABAPObject, DataReference, FieldSymbol, HashedTable, Structure, Table, TableAccessType, TableRowType} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {compareRows, sort} from "./sort";
import {ABAP} from "..";

declare const abap: ABAP;

/** true when the row has the key of a row already in the table, for one of its unique secondary keys */
export function duplicateSecondaryKey(table: Table | HashedTable, row: any): boolean {
  const unique = (table.getOptions()?.secondary ?? []).filter(k => k.isUnique === true);
  if (unique.length === 0) {
    return false;
  }
  const value = row instanceof FieldSymbol ? row.getPointer() : row;
  return unique.some(k => table.array().some(existing => compareRows(existing, value, k.keyFields) === 0));
}

function duplicateKey(): never {
  if (abap.Classes["CX_SY_ITAB_DUPLICATE_KEY"] !== undefined) {
    throw new abap.Classes["CX_SY_ITAB_DUPLICATE_KEY"]();
  }
  throw new Error("ITAB_DUPLICATE_KEY");
}

export interface IInsertInternalOptions {
  index?: INumeric,
  initial?: boolean,
  data?: INumeric | ICharacter | Structure | ABAPObject | Table | HashedTable | FieldSymbol | string,
  table: Table | HashedTable | FieldSymbol,
  referenceInto?: DataReference,
  assigning?: FieldSymbol,
  noClone?: boolean,
  lines?: boolean,
  /** INSERT wa INTO itab, without INDEX and TABLE: in a LOOP over itab, in front of the current row */
  currentRow?: boolean,
}

export function insertInternal(options: IInsertInternalOptions): void {
  if (options.table instanceof FieldSymbol) {
    if (options.table.getPointer() === undefined) {
      throw new Error("GETWA_NOT_ASSIGNED");
    }
    options.table = options.table.getPointer() as Table;
  }
  if (options.data instanceof FieldSymbol) {
    if (options.data.getPointer() === undefined) {
      throw new Error("GETWA_NOT_ASSIGNED");
    }
    options.data = options.data.getPointer();
  }

  // a duplicate of a unique secondary key is not inserted: CX_SY_ITAB_DUPLICATE_KEY
  if (options.lines !== true && options.initial !== true && options.data !== undefined
      && duplicateSecondaryKey(options.table, options.data)) {
    duplicateKey();
  }

  let data = options.data;
  if (typeof data === "string") {
    const tmp = options.table.getRowType().clone() as ICharacter;
    tmp.set(data);
    data = tmp;
  }

  const tableOptions = options.table.getOptions();

  const isSorted = tableOptions?.primaryKey?.type === TableAccessType.sorted
    || tableOptions?.primaryKey?.type === TableAccessType.hashed;

  if (isSorted === true
      && !(options.table instanceof HashedTable)
      && options.index === undefined
      && options.lines !== true) {
    const insert = options.initial === true ? options.table.getRowType() : data;
    if (insert !== undefined) {
      const result = options.table.insertSorted(
        insert as TableRowType,
        (a, b) => compareRows(a, b, tableOptions?.primaryKey?.keyFields || []),
        tableOptions?.primaryKey?.isUnique === true,
        options.noClone);
      if (result.subrc === 0) {
        if (options.assigning) {
          options.assigning.assign(result.value);
        }
        if (options.referenceInto) {
          options.referenceInto.assign(result.value);
        }
      }
      abap.builtin.sy.get().subrc.set(result.subrc);
      return;
    }
  }

  if (data && (options.index || (options.currentRow === true && options.table instanceof Table && options.table.getLoopCount?.() === 1))) {
    const index = options.index ? options.index.get() - 1 : (options.table as Table).getCurrentLoopIndex();
    if (index > options.table.getArrayLength()) {
      // an index past the row after the last one inserts nothing
      abap.builtin.sy.get().subrc.set(4);
      return;
    }
    const val = options.table.insertIndex(data as any, index);
    if (options.assigning) {
      options.assigning.assign(val);
    }
  } else if (options.lines
      && (options.data instanceof Table
      || options.data instanceof HashedTable)) {
    if (options.table instanceof HashedTable) {
      for (const source of options.data.array()) {
        const result = options.table.insert(source);
        if (result.subrc !== 0) {
          throw new Error("ITAB_DUPLICATE_KEY");
        }
      }
    } else if (options.table.getOptions()?.primaryKey?.type === TableAccessType.sorted) {
      // into a sorted table: the block goes in front of the rows with the same key, in its own order
      const key = options.table.getOptions().primaryKey!;
      for (const source of [...options.data.array()].reverse()) {
        const result = options.table.insertSorted(source, (a, b) => compareRows(a, b, key.keyFields), key.isUnique === true);
        if (result.subrc !== 0) {
          duplicateKey();
        }
      }
    } else {
      for (const i of options.data.array()) {
        options.table.append(i);
      }
    }
  } else if (options.initial === true) {
    if (options.table instanceof HashedTable) {
      const {value: val, subrc: subrc} = options.table.insert(options.table.getRowType());
      if (subrc === 0) {
        if (options.assigning) {
          options.assigning.assign(val);
        }
        if (options.referenceInto) {
          options.referenceInto.assign(val);
        }
      }
      abap.builtin.sy.get().subrc.set(subrc);
      return;
    }
    let index = options.table.getArrayLength();
    if (options.index) {
      index = options.index.get() - 1;
    }
    const val = options.table.insertIndex(options.table.getRowType(), index);
    if (options.assigning) {
      options.assigning.assign(val);
    }
    if (options.referenceInto) {
      options.referenceInto.assign(val);
    }
  } else if (options.table instanceof HashedTable && data) {
    const {value: val, subrc: subrc} = options.table.insert(data as any);
    if (subrc === 0) {
      if (options.assigning) {
        options.assigning.assign(val);
      }
      if (options.referenceInto) {
        options.referenceInto.assign(val);
      }
    }
    abap.builtin.sy.get().subrc.set(subrc);
    return;
  } else if (data) {
    // todo, for now it just appends, this is not correct, but currently the table type is not known
    const val = options.table.insertIndex(data as any, options.table.getArrayLength(), options.noClone);
    if (options.assigning) {
      options.assigning.assign(val);
    }
    if (options.referenceInto) {
      options.referenceInto.assign(val);
    }
  }

  abap.builtin.sy.get().subrc.set(0);

  if (isSorted && !(options.table instanceof HashedTable)) {
// slow, but works for now
    let by = tableOptions?.primaryKey?.keyFields?.map(f => {
      return {component: f.toLowerCase()};
    });
    if (by?.length === 1 && by[0].component === "table_line") {
      by = [];
    }
    if (by && by.length > 0) {
      sort(options.table, {by: by, skipSortedCheck: true});
    } else {
      sort(options.table, {skipSortedCheck: true});
    }
  }

}
