import {Context} from "../context";
import {Structure} from "../types";
import {ICharacter} from "../types/_character";

export interface IInsertDatabaseOptions {
  values: Structure,
}

export class InsertDatabase {
  private readonly context: Context;

  public constructor(context: Context) {
    this.context = context;
  }

  public insertDatabase(table: string | ICharacter, options: IInsertDatabaseOptions): number {
    if (this.context.db === undefined) {
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

    if (typeof table !== "string") {
      table = table.get();
    }

    const {subrc, dbcnt} = this.context.db.insert(table, columns, values);

    // @ts-ignore
    abap.builtin.sy.get().subrc.set(subrc);
    // @ts-ignore
    abap.builtin.sy.get().dbcnt.set(dbcnt);
    return subrc;
  }
}