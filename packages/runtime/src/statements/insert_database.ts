import {Structure} from "../types";
import {ICharacter} from "../types/_character";

export interface IInsertDatabaseOptions {
  values: Structure,
}

export function insertDatabase(table: string | ICharacter, options: IInsertDatabaseOptions): number {
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

  let subrc = 0;
  try {
    this.db.exec(sql);
  } catch (error) {
// eg "UNIQUE constraint failed" errors
    subrc = 4;
  }

  // @ts-ignore
  abap.builtin.sy.get().subrc.set(subrc);
  return subrc;
}