import {ABAPObject, FieldSymbol, Structure, Table} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export interface IInsertInternalOptions {
  index?: INumeric,
  initial?: boolean,
  data?: INumeric | ICharacter | Structure | ABAPObject | Table,
  table: Table,
  assigning?: FieldSymbol,
  lines?: boolean,
}

export function insertInternal(
  options: IInsertInternalOptions): void {

  if (options.data && options.index) {
    const index = options.index.get() - 1;
    const val = options.table.insertIndex(options.data, index);
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
  } else if (options.data) {
// todo, for now it just appends, this is not correct, but currently the table type is not known
    const val = options.table.insertIndex(options.data, options.table.array().length);
    if (options.assigning) {
      options.assigning.assign(val);
    }
  }

}