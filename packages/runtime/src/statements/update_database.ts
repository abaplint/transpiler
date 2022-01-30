import {FieldSymbol, Structure, Table} from "../types";
import {ICharacter} from "../types/_character";

export interface IInsertDatabaseOptions {
  from?: Structure | FieldSymbol,
  table?: Table | FieldSymbol,
}

export function updateDatabase(table: string | ICharacter, options: IInsertDatabaseOptions): number {
  if (this.db === undefined) {
    throw new Error("Runtime, database not initialized");
  }

  if (options.table instanceof FieldSymbol) {
    options.table = options.table.getPointer() as Table;
  }
  if (options.from instanceof FieldSymbol) {
    options.from = options.from.getPointer() as Structure;
  }

  if (typeof table !== "string") {
    table = table.get();
  }

  const sql = "todo";

  let subrc = 0;
  try {
    this.db.exec(sql);
  } catch (error) {
    subrc = 4;
  }

  // @ts-ignore
  abap.builtin.sy.get().subrc.set(subrc);
  return subrc;
}