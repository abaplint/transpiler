import {FieldSymbol, Structure, Table} from "../types";
import {ICharacter} from "../types/_character";
import {insertDatabase} from "./insert_database";
import {updateDatabase} from "./update_database";

export interface IInsertDatabaseOptions {
  from?: Structure | FieldSymbol,
  table?: Table | FieldSymbol,
}

export function modifyDatabase(table: string | ICharacter, options: IInsertDatabaseOptions): void {
  if (this.db === undefined) {
    throw new Error("Runtime, database not initialized");
  }
  if (options.table instanceof FieldSymbol) {
    options.table = options.table.getPointer() as Table;
  }
  if (options.from instanceof FieldSymbol) {
    options.from = options.from.getPointer() as Structure;
  }

  if (options.table) {
    for (const row of options.table.array()) {
      const subrc = insertDatabase.bind(this)(table, {values: row});
      if (subrc !== 0) {
        updateDatabase.bind(this)(table, {from: row});
      }
    }
  } else if (options.from) {
    const subrc = insertDatabase.bind(this)(table, {values: options.from});
    if (subrc !== 0) {
      updateDatabase.bind(this)(table, {from: options.from});
    }
  } else {
    throw "modifyDatabase todo";
  }

}