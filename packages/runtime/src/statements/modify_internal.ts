import {FieldSymbol, Integer, Table} from "../types";
import {INumeric} from "../types/_numeric";
import {deleteInternal} from "./delete_internal";
import {insertInternal} from "./insert_internal";
import {readTable} from "./read_table";
import {ABAP} from "..";

declare const abap: ABAP;

export interface IModifyInternalOptions {
  index: INumeric,
  from: any,
  where?: (i: any) => boolean,
  transporting?: string[],
}

export function modifyInternal(table: Table, options: IModifyInternalOptions): void {

  let found = false;

//  console.dir(options);

  if (options.index) {
    const index = options.index.get() - 1;
    const element = table.array()[index];
    found = element !== undefined;
    if (found) {
      element.set(options.from);
      /*
      table.deleteIndex(index);
      table.insertIndex(options.from, index);
      */
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
        for (const t of options.transporting) {
          fs.get()[t].set(options.from.get()[t]);
        }
      }
      index++;
    }
  } else if (options.from) {
    const readResult = readTable(table, {from: options.from});
    if (readResult.subrc === 0) {
      deleteInternal(table, {index: new Integer().set(readResult.foundIndex)});
    }
    insertInternal({table, data: options.from});
  }

  const subrc = found ? 0 : 4;
  abap.builtin.sy.get().subrc.set(subrc);
}