import {clone} from "../clone";
import {eq, ne} from "../compare";
import {ABAPObject, DataReference, FieldSymbol, HashedTable, Structure, Table, TableAccessType} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {readTable} from "./read_table";
import {sort} from "./sort";

export interface IInsertInternalOptions {
  index?: INumeric,
  initial?: boolean,
  data?: INumeric | ICharacter | Structure | ABAPObject | Table | FieldSymbol | string,
  table: Table | HashedTable | FieldSymbol,
  referenceInto?: DataReference,
  assigning?: FieldSymbol,
  noClone?: boolean,
  lines?: boolean,
}

export function insertInternal(options: IInsertInternalOptions): void {
  if (options.table instanceof FieldSymbol) {
    if (options.table.getPointer() === undefined) {
      throw new Error("GETWA_NOT_ASSIGNED");
    }
    options.table = options.table.getPointer() as Table;
  } else if (options.data instanceof FieldSymbol) {
    if (options.data.getPointer() === undefined) {
      throw new Error("GETWA_NOT_ASSIGNED");
    }
    options.data = options.data.getPointer();
  }

  const tableOptions = options.table.getOptions();

  let isSorted = tableOptions?.primaryKey?.type === TableAccessType.sorted
    || tableOptions?.primaryKey?.type === TableAccessType.hashed;

  if (options.table instanceof HashedTable) {
    isSorted = false;
  } else if (isSorted) {
    const insert = options.data instanceof Structure ? options.data.get() : {table_line: options.data};

    let compare = (row: any): boolean => {
      for (const key of tableOptions?.primaryKey?.keyFields || []) {
        if (key.includes("-")) {
          const [first, second] = key.split("-");
          if (ne(row[first.toLowerCase()].get()[second.toLowerCase()], insert[first.toLowerCase()].get()[second.toLowerCase()])) {
            return false;
          }
        } else {
          if (ne(row[key.toLowerCase()], insert[key.toLowerCase()])) {
            return false;
          }
        }
      }
      return true;
    };

    if (tableOptions.primaryKey?.isUnique === true) {

      const withKeyValue: {key: (i: any) => any, value: any}[] = [];
      let binary = false;
      const data = options?.data;
      if (data instanceof Structure) {
        const fieldName = tableOptions.primaryKey.keyFields[0].toLowerCase();
        if (fieldName !== "table_line" && fieldName.includes("-") === false) {
          withKeyValue.push({key: (i) => {return i[fieldName];}, value: data.get()[fieldName]});
          binary = true;
        }
      } else {
        compare = (row: any): boolean => {
          // @ts-ignore
          return eq(row.table_line, options.data);
        };
      }

      readTable(options.table, {withKey: compare, withKeyValue: withKeyValue, binarySearch: binary});
      // @ts-ignore
      if (abap.builtin.sy.get().subrc.get() === 0) {
        // @ts-ignore
        abap.builtin.sy.get().subrc.set(4);
        return;
      }
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
    } else {
      for (const i of options.data.array()) {
        options.table.append(i);
      }
    }
  } else if (options.initial === true) {
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
    const {value: val, subrc: subrc} = options.table.insert(data);
    if (options.assigning) {
      options.assigning.assign(val);
    }
    if (options.referenceInto) {
      options.referenceInto.assign(val);
    }
    // @ts-ignore
    abap.builtin.sy.get().subrc.set(subrc);
    return;
  } else if (data) {
    // todo, for now it just appends, this is not correct, but currently the table type is not known
    const val = options.table.insertIndex(data, options.table.getArrayLength(), options.noClone);
    if (options.assigning) {
      options.assigning.assign(val);
    }
    if (options.referenceInto) {
      options.referenceInto.assign(val);
    }
  }

  // @ts-ignore
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