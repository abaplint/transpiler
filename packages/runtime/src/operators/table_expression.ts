import {throwError} from "../throw_error";
import {HashedTable, Integer, Table} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {parse} from "./_parse";

export interface ITableExpressionOptions {
  index?: INumeric | ICharacter | string | Integer | number
}

export function tableExpression(source: Table | HashedTable, options: ITableExpressionOptions) {
  let found;
  if (options.index) {
    found = source.array()[ parse(options.index) - 1 ];
  } else {
    throw new Error("TableExpression: todo");
  }

  if (found === undefined) {
    throwError("CX_SY_ITAB_LINE_NOT_FOUND");
  }

  return found;
}