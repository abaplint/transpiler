import {clone} from "../clone";
import {ne} from "../compare";
import {ABAPObject, DataReference, FieldSymbol, Structure, Table, TableAccessType} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {readTable} from "./read_table";
import {sort} from "./sort";

export interface IInsertInternalOptions {
  index?: INumeric,
  initial?: boolean,
  data?: INumeric | ICharacter | Structure | ABAPObject | Table | string,
  table: Table | FieldSymbol,
  referenceInto?: DataReference,
  assigning?: FieldSymbol,
  lines?: boolean,
}

export function insertInternal(options: IInsertInternalOptions): void {
  if (options.table instanceof FieldSymbol) {
    options.table = options.table.getPointer() as Table;
  }

  const tableOptions = options.table.getOptions();
  const isSorted = tableOptions?.type === TableAccessType.sorted || tableOptions?.type === TableAccessType.hashed;

  if (isSorted) {
    const insert = options.data instanceof Structure ? options.data.get() : {table_line: options.data};
    const compare = (row: any): boolean => {
      for (const key of tableOptions?.keyFields || []) {
        if (ne(row[key.toLowerCase()], insert[key.toLowerCase()])) {
          return false;
        }
      }
      return true;
    };
    readTable(options.table, {withKey: compare});
    // @ts-ignore
    if (abap.builtin.sy.get().subrc.get() === 0) {
      // @ts-ignore
      abap.builtin.sy.get().subrc.set(4);
      return;
    }
  }

  let data = options.data;
  if (typeof data === "string") {
    const tmp = clone(options.table.getRowType()) as ICharacter;
    tmp.set(data);
    data = tmp;
  }

  if (data && options.index) {
    const index = options.index.get() - 1;
    const val = options.table.insertIndex(data, index);
    if (options.assigning) {
      options.assigning.assign(val);
    }
  } else if (options.lines && options.data instanceof Table) {
    for (const i of options.data.array()) {
      options.table.append(i);
    }
  } else if (options.initial === true) {
    let index = options.table.array().length;
    if (options.index) {
      index = options.index.get() - 1;
    }
    const val = options.table.insertIndex(options.table.getRowType(), index);
    if (options.assigning) {
      options.assigning.assign(val);
    }
  } else if (data) {
// todo, for now it just appends, this is not correct, but currently the table type is not known
    const val = options.table.insertIndex(data, options.table.array().length);
    if (options.assigning) {
      options.assigning.assign(val);
    }
    if (options.referenceInto) {
      options.referenceInto.assign(val);
    }
  }

  // @ts-ignore
  abap.builtin.sy.get().subrc.set(0);

  if (isSorted) {
// slow, but works for now
    const by = tableOptions?.keyFields?.map(f => {return {component: f.toLowerCase()}; });
    sort(options.table, {by: by});
  }

}