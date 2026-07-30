import {Context} from "../context";
import {FieldSymbol, Structure, Table} from "../types";
import {ICharacter} from "../types/_character";
import {insertDatabase} from "./insert_database";
import {updateDatabase} from "./update_database";
import {ABAP} from "..";

declare const abap: ABAP;

export interface IModifyDatabaseOptions {
  values?: Structure | FieldSymbol,
  table?: Table | FieldSymbol,
}

export async function modifyDatabase(table: string | ICharacter, options: IModifyDatabaseOptions, context: Context) {
  if (options.table instanceof FieldSymbol) {
    options.table = options.table.getPointer() as Table;
  }
  if (options.values instanceof FieldSymbol) {
    options.values = options.values.getPointer() as Structure;
  }

  if (options.table) {
    let subrc = 0;
    let dbcnt = 0;
    for (const row of options.table.array()) {
      const insertSubrc = await insertDatabase(table, {values: row}, context);
      if (insertSubrc !== 0) {
        await updateDatabase(table, {from: row}, context);
      }
      subrc = Math.max(subrc, abap.builtin.sy.get().subrc.get());
      dbcnt += abap.builtin.sy.get().dbcnt.get();
    }
    abap.builtin.sy.get().subrc.set(subrc);
    abap.builtin.sy.get().dbcnt.set(dbcnt);
  } else if (options.values) {
    const subrc = await insertDatabase(table, {values: options.values}, context);
    if (subrc !== 0) {
      await updateDatabase(table, {from: options.values}, context);
    }
  } else {
    throw new Error("modifyDatabase todo");
  }
}
