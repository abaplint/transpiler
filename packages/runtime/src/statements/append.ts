import {Table, TableRowType} from "../types";

export interface IAppendOptions {
  source: TableRowType,
  target: Table,
  lines?: boolean,
}

export function append(input: IAppendOptions) {
  if (input.lines === true && input.source instanceof Table) {
    for (const a of input.source.array()) {
      input.target.append(a);
    }
  } else {
    input.target.append(input.source);
  }
}