import {Table, TableRowType} from "../types";

export function append(input: {source: TableRowType, target: Table}) {
  input.target.append(input.source);
}