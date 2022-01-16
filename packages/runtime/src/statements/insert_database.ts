import {Structure} from "../types";
import {ICharacter} from "../types/_character";

export interface IInsertDatabaseOptions {
  values: Structure,
}

export function insertDatabase(table: string | ICharacter, options: IInsertDatabaseOptions): void {
  if (this.db === undefined) {
    throw new Error("Runtime, database not initialized");
  }

  const columns: string[] = [];
  const values: string[] = [];

  const structure = options.values.get();
  for (const k of Object.keys(structure)) {
    columns.push(k);
    // todo, integers should not be surrounded by '"'?
    values.push('"' + structure[k].get() + '"');
  }

  const sql = `INSERT INTO ${table} (${columns.join(",")}) VALUES (${values.join(",")})`;
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