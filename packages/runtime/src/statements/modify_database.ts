import {Context} from "../context";
import {FieldSymbol, Structure, Table} from "../types";
import {ICharacter} from "../types/_character";
import {InsertDatabase} from "./insert_database";
import {UpdateDatabase} from "./update_database";

export interface IModifyDatabaseOptions {
  from?: Structure | FieldSymbol,
  table?: Table | FieldSymbol,
}

export class ModifyDatabase {
  private readonly context: Context;

  public constructor(context: Context) {
    this.context = context;
  }

  public async modifyDatabase(table: string | ICharacter, options: IModifyDatabaseOptions) {
    if (options.table instanceof FieldSymbol) {
      options.table = options.table.getPointer() as Table;
    }
    if (options.from instanceof FieldSymbol) {
      options.from = options.from.getPointer() as Structure;
    }

    const insert = new InsertDatabase(this.context);
    const update = new UpdateDatabase(this.context);
    if (options.table) {
      for (const row of options.table.array()) {
        const subrc = await insert.insertDatabase(table, {values: row});
        if (subrc !== 0) {
          await update.updateDatabase(table, {from: row});
        }
      }
    } else if (options.from) {
      const subrc = await insert.insertDatabase(table, {values: options.from});
      if (subrc !== 0) {
        await update.updateDatabase(table, {from: options.from});
      }
    } else {
      throw "modifyDatabase todo";
    }

  }
}