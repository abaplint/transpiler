import {searchWithKeyPromise} from "../statements/read_table";
import {throwError} from "../throw_error";
import {HashedTable, Integer, Table} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {parse} from "./_parse";

export interface ITableExpressionOptions {
  index?: INumeric | ICharacter | string | Integer | number
  // single function, evaluates full condition
  withKey?: (i: any) => Promise<boolean>,
  usesTableLine?: boolean
}

export let foundIndex: number = 0;

export async function tableExpression(source: Table | HashedTable, options: ITableExpressionOptions) {
  let found;
  if (options.index) {
    foundIndex = parse(options.index) - 1;
    found = source.array()[ foundIndex ];
  } else if (options.withKey) {
    const search = await searchWithKeyPromise(source.array(), options.withKey, 0, options?.usesTableLine);
    found = search.found;
    foundIndex = search.foundIndex;
  } else {
    throw new Error("TableExpression runtime: todo");
  }

  if (found === undefined) {
    throwError("CX_SY_ITAB_LINE_NOT_FOUND");
  }

  return found;
}