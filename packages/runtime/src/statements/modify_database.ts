import {FieldSymbol, Structure, Table} from "../types";
import {ICharacter} from "../types/_character";

export interface IInsertDatabaseOptions {
  from?: Structure | FieldSymbol,
  table?: Table | FieldSymbol,
}

export function modifyDatabase(table: string | ICharacter, _options: IInsertDatabaseOptions): void {
  if (this.db === undefined) {
    throw new Error("Runtime, database not initialized");
  }

  const columns: string[] = [];
  const values: string[] = [];

  const sql = `TODO INSERT INTO ${table} (${columns.join(",")}) VALUES (${values.join(",")})`;
//  console.dir(sql);

  try {
    this.db.exec(sql);
    // @ts-ignore
    abap.builtin.sy.get().subrc.set(0);
  } catch (error) {
// eg "UNIQUE constraint failed" errors
    // @ts-ignore
    abap.builtin.sy.get().subrc.set(4);
  }

}