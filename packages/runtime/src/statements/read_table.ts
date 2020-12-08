import {FieldSymbol, Table} from "../types";
import {INumeric} from "../types/_numeric";

export interface IReadTableOptions {
  index?: INumeric | number,
  withKey?: (i: any) => boolean,
}

export function readTable(table: Table | FieldSymbol, options?: IReadTableOptions) {

  let found: any = undefined;

  const arr = table.array();

  if (options?.index) {
    let index = options.index;
    if (typeof index !== "number") {
      index = index.get();
    }

    found = arr[index - 1];
  } else if (options?.withKey) {
    // eslint-disable-next-line @typescript-eslint/prefer-for-of
    for (let i = 0; i < arr.length; i++) {
      if (options.withKey(arr[i]) === true) {
        found = arr[i];
        break;
      }
    }

  } else {
    throw new Error("runtime, readTable, unexpected input");
  }

  const subrc = found ? 0 : 4;
  // @ts-ignore
  abap.builtin.sy.get().subrc.set(subrc);

  return found;
}