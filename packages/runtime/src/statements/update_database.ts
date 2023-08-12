import {Context} from "../context";
import {buildDbTableName} from "../prefix";
import {FieldSymbol, Structure, Table} from "../types";
import {ICharacter} from "../types/_character";
import {toValue} from "./insert_database";

export interface IUpdateDatabaseOptions {
  from?: Structure | FieldSymbol,
  table?: Table | FieldSymbol,
  set?: string[],
  where?: string,
}

export class UpdateDatabase {
  private readonly context: Context;

  public constructor(context: Context) {
    this.context = context;
  }

  public async updateDatabase(table: string | ICharacter, options: IUpdateDatabaseOptions) {
    if (options.table instanceof FieldSymbol) {
      options.table = options.table.getPointer() as Table;
    }
    if (options.from instanceof FieldSymbol) {
      options.from = options.from.getPointer() as Structure;
    }

    if (typeof table !== "string") {
      table = table.get();
    }

    // @ts-ignore
    const keys: string[] = abap.DDIC[table.toUpperCase()].keyFields;
    const where: string[] = [];
    const set: string[] = [];

    if (options.from) {
      const structure = options.from.get();
      for (const k of Object.keys(structure)) {
        const str = k + " = " + toValue(structure[k].get());
        if (keys.includes(k.toUpperCase())) {
          where.push(str);
        } else {
          set.push(str);
        }
      }
    } else if (options.set) {
      if (options.where) {
        where.push(options.where);
      }
      set.push(...options.set);
    } else {
      throw "updateDatabase, todo";
    }

    const {subrc, dbcnt} = await this.context.defaultDB().update({
      table: buildDbTableName(table),
      where: where.join(" AND "),
      set,
    });

    // @ts-ignore
    abap.builtin.sy.get().subrc.set(subrc);
    // @ts-ignore
    abap.builtin.sy.get().dbcnt.set(dbcnt);
    return subrc;
  }
}