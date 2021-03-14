import {FieldSymbol, Table, TableRowType} from "../types";

export interface IAppendOptions {
  source: TableRowType,
  target: Table,
  lines?: boolean,
  assigning?: FieldSymbol,
}

export function append(input: IAppendOptions) {
  if (input.lines === true && input.source instanceof Table) {
    for (const a of input.source.array()) {
      input.target.append(a);
    }
  } else {
    const val = input.target.append(input.source);
    if (input.assigning) {
      if (val instanceof FieldSymbol) {
        input.assigning.assign(val.getPointer());
      } else {
        input.assigning.assign(val);
      }
    }
  }
}