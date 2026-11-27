import {DataReference, FieldSymbol, Table, TableAccessType, TableRowType} from "../types";
import {compareRows} from "./sort";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {ABAP} from "..";

declare const abap: ABAP;

export interface IAppendOptions {
  source: TableRowType | FieldSymbol | Table,
  target: Table | FieldSymbol | undefined,
  lines?: boolean,
  assigning?: FieldSymbol,
  referenceInto?: DataReference,
  from?: ICharacter | INumeric,
  to?: ICharacter | INumeric,
}

/** APPEND to a sorted table must keep the sort order of the primary key, and a unique key unique */
function checkSorted(table: Table, row: any) {
  const key = table.getOptions()?.primaryKey;
  if (key?.type !== TableAccessType.sorted) {
    return;
  }
  const array = table.array();
  const last = array[array.length - 1];
  if (last === undefined) {
    return;
  }
  const result = compareRows(row instanceof FieldSymbol ? row.getPointer() : row, last, key.keyFields);
  if (result < 0) {
    throw new Error("ITAB_ILLEGAL_SORT_ORDER");
  } else if (result === 0 && key.isUnique === true) {
    // the name a 7.40 system gives this runtime error
    throw new Error("ITAB_DUPLICATE_KEY_IDX_OP");
  }
}

export function append(input: IAppendOptions) {
  if (input.target instanceof FieldSymbol) {
    input.target = input.target.getPointer() as Table | undefined;
    if (input.target === undefined) {
      throw new Error("Field symbol not assigned");
    }
  }

  if (input.source instanceof FieldSymbol) {
    input.source = input.source.getPointer() as TableRowType;
  }

  if (input.target === undefined) {
    // short APPEND, use header field
    if (!(input.source instanceof Table)) {
      throw new Error("APPEND, header, table");
    }
    checkSorted(input.source, input.source.getHeader());
    input.source.append(input.source.getHeader());
    abap.builtin.sy.get().tabix.set(input.source.array().length);
    return;
  } else if (input.lines === true && input.source instanceof Table) {
    let from = 1;
    if (input.from) {
      from = parseInt(input.from.get() + "", 10);
    }
    let to = input.source.array().length;
    if (input.to) {
      to = parseInt(input.to.get() + "", 10);
    }

    let index = 1;
    for (const a of input.source.array()) {
      if (index < from || index > to) {
        index++;
        continue;
      }
      checkSorted(input.target, a);
      input.target.append(a);
      index++;
    }
  } else {
    checkSorted(input.target, input.source);
    const val = input.target.append(input.source);
    if (input.assigning) {
      if (val instanceof FieldSymbol) {
        input.assigning.assign(val.getPointer());
      } else {
        input.assigning.assign(val);
      }
    } else if (input.referenceInto) {
      if (val instanceof FieldSymbol) {
        input.referenceInto.assign(val.getPointer());
      } else {
        input.referenceInto.assign(val);
      }
    }
  }

  abap.builtin.sy.get().tabix.set(input.target.array().length);
}