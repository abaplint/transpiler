import {FieldSymbol, HashedTable, Integer, Table} from "../types";
import {INumeric} from "../types/_numeric";
import {readTable} from "./read_table";
import {ABAP} from "..";

declare const abap: ABAP;

export interface IModifyInternalOptions {
  index: INumeric,
  from: any,
  where?: (i: any) => boolean,
  transporting?: string[],
}

function component(row: any, name: string): any {
  let value = row;
  for (const part of name.split("-")) {
    value = value.get()[part];
  }
  return value;
}

function modifyRow(row: any, options: IModifyInternalOptions): void {
  if (options.transporting) {
    for (const t of options.transporting) {
      component(row, t).set(component(options.from, t));
    }
  } else {
    row.set(options.from);
  }
}

export function modifyInternal(table: Table | HashedTable, options: IModifyInternalOptions): void {

  let found = false;

  if (options.index) {
    const index = options.index.get() - 1;
    const element = table.array()[index];
    found = element !== undefined;
    if (found) {
      modifyRow(element, options);
    }
  } else if (options.where && options.transporting && options.from) {
    let index = 1;
    const fs = new FieldSymbol();
    while (index <= table.array().length) {
      const currentIndex = new Integer().set(index);
      const readResult = readTable(table, {
        withKey: options.where,
        assigning: fs,
        index: currentIndex});
      if (readResult.subrc === 0) {
        found = true;
        modifyRow(fs.getPointer(), options);
      }
      index++;
    }
  } else if (options.from) {
    // the key is unchanged, so the row is modified in place, keeping position and hash
    const fs = new FieldSymbol();
    found = readTable(table, {from: options.from, assigning: fs}).subrc === 0;
    if (found) {
      modifyRow(fs.getPointer(), options);
    }
  }

  const subrc = found ? 0 : 4;
  abap.builtin.sy.get().subrc.set(subrc);
}
