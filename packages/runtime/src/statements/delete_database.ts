import {Context} from "../context";
import {FieldSymbol, Structure, Table} from "../types";
import {ICharacter} from "../types/_character";

export interface IDeleteDatabaseOptions {
  from?: Structure | FieldSymbol,
  table?: Table | FieldSymbol,
}

export class DeleteDatabase {
  private readonly context: Context;

  public constructor(context: Context) {
    this.context = context;
  }

  public deleteDatabase(table: string | ICharacter, options: IDeleteDatabaseOptions): void {
    if (this.context.db === undefined) {
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

    if (options.table) {
      for (const row of options.table.array()) {
        this.deleteDatabase(table, {from: row});
      }
    } else if (options.from) {
      let where: string[] | string = [];

      const structure = options.from.get();
      for (const k of Object.keys(structure)) {
        // todo, integers should not be surrounded by '"'?
        const str = k + ' = "' + structure[k].get() + '"';
        where.push(str);
      }
      where = where.join(" AND ");

      const {subrc, dbcnt} = this.context.db.delete({table, where});

      // @ts-ignore
      abap.builtin.sy.get().subrc.set(subrc);
      // @ts-ignore
      abap.builtin.sy.get().dbcnt.set(dbcnt);
    } else {
      throw "deleteDatabase todo";
    }
  }
}