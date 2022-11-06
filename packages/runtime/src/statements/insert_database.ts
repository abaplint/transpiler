import {Context} from "../context";
import {Structure, Table} from "../types";
import {ICharacter} from "../types/_character";

export interface IInsertDatabaseOptions {
  values?: Structure,
  table?: Table,
}

export class InsertDatabase {
  private readonly context: Context;

  public constructor(context: Context) {
    this.context = context;
  }

  public async insertDatabase(table: string | ICharacter, options: IInsertDatabaseOptions) {
    const columns: string[] = [];
    const values: string[] = [];

    if (options.values === undefined && options.table === undefined) {
      throw "insertDatabase, wrong input";
    }

    if (options.table !== undefined) {
      for (const row of options.table.array()) {
        await this.insertDatabase(table, {values: row});
      }
//     todo, set sy-subrc
//     todo, set sy-dbcnt
      return;
    }

    const structure = options.values!.get();
    for (const k of Object.keys(structure)) {
      columns.push(k);
      // todo, integers should not be surrounded by '"'?
      values.push('"' + structure[k].get().replace(/"/g, "\"\"") + '"');
    }

    if (typeof table !== "string") {
      table = table.get();
    }

    const {subrc, dbcnt} = await this.context.defaultDB().insert({table, columns, values});

    // @ts-ignore
    abap.builtin.sy.get().subrc.set(subrc);
    // @ts-ignore
    abap.builtin.sy.get().dbcnt.set(dbcnt);
    return subrc;
  }
}