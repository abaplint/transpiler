import {Context} from "../context.js";
import {FieldSymbol, Structure, Table} from "../types/index.js";
import {ICharacter} from "../types/_character.js";

export interface IDeleteDatabaseOptions {
  from?: Structure | FieldSymbol,
  where?: string,
  table?: Table | FieldSymbol,
}

export class DeleteDatabase {
  private readonly context: Context;

  public constructor(context: Context) {
    this.context = context;
  }

  public async deleteDatabase(table: string | ICharacter, options: IDeleteDatabaseOptions) {
    if (options.table instanceof FieldSymbol) {
      options.table = options.table.getPointer() as Table;
    }
    if (options.from instanceof FieldSymbol) {
      options.from = options.from.getPointer() as Structure;
    }
    if (typeof table !== "string") {
      table = table.get().trimEnd();
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

      const {subrc, dbcnt} = await this.context.defaultDB().delete({table, where});

      // @ts-ignore
      abap.builtin.sy.get().subrc.set(subrc);
      // @ts-ignore
      abap.builtin.sy.get().dbcnt.set(dbcnt);
    } else if (options.where) {
      const {subrc, dbcnt} = await this.context.defaultDB().delete({table, where: options.where});

      // @ts-ignore
      abap.builtin.sy.get().subrc.set(subrc);
      // @ts-ignore
      abap.builtin.sy.get().dbcnt.set(dbcnt);
    } else {
      throw "deleteDatabase todo";
    }
  }
}