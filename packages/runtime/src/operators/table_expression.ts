import {HashedTable, Integer, Table} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {parse} from "./_parse";

export interface ITableExpressionOptions {
  index?: INumeric | ICharacter | string | Integer | number
}

export function tableExpression(source: Table | HashedTable, options: ITableExpressionOptions) {
  if (options.index) {
    return source.array()[ parse(options.index) - 1 ];
  }

  throw new Error("TableExpression: todo");
}