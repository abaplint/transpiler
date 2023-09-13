import {Context} from "../context";
import {FieldSymbol, Structure, Table} from "../types";
import {ICharacter} from "../types/_character";
import {insertDatabase} from "./insert_database";
import {updateDatabase} from "./update_database";

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
    for (const row of options.table.array()) {
      const subrc = await insertDatabase(table, {values: row}, context);
      if (subrc !== 0) {
        await updateDatabase(table, {from: row}, context);
      }
    }
  } else if (options.values) {
    const subrc = await insertDatabase(table, {values: options.values}, context);
    if (subrc !== 0) {
      await updateDatabase(table, {from: options.values}, context);
    }
  } else {
    throw "modifyDatabase todo";
  }
}