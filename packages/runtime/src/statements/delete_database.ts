import {FieldSymbol, Structure, Table} from "../types";
import {ICharacter} from "../types/_character";

export interface IInsertDatabaseOptions {
  from?: Structure | FieldSymbol,
  table?: Table | FieldSymbol,
}

export function deleteDatabase(table: string | ICharacter, options: IInsertDatabaseOptions): void {
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
      deleteDatabase.bind(this)(table, {from: row});
    }
  } else if (options.from) {
    const where: string[] = [];

    const structure = options.from.get();
    for (const k of Object.keys(structure)) {
      // todo, integers should not be surrounded by '"'?
      const str = k + ' = "' + structure[k].get() + '"';
      where.push(str);
    }

    const sql = `DELETE FROM ${table} WHERE ${where.join(" AND ")}`;

    let subrc = 0;
    try {
      this.db.exec(sql);
    } catch (error) {
      subrc = 4;
    }

    // @ts-ignore
    abap.builtin.sy.get().subrc.set(subrc);
  } else {
    throw "deleteDatabase todo";
  }

}