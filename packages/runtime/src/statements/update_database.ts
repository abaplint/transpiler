import {Context} from "../context";
import {FieldSymbol, Structure, Table} from "../types";
import {ICharacter} from "../types/_character";

export interface IUpdateDatabaseOptions {
  from?: Structure | FieldSymbol,
  table?: Table | FieldSymbol,
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
        // todo, integers should not be surrounded by '"'?
        const str = k + ' = "' + structure[k].get() + '"';
        if (keys.includes(k.toUpperCase())) {
          where.push(str);
        } else {
          set.push(str);
        }
      }
    } else {
      throw "updateDatabase, todo";
    }

    const {subrc, dbcnt} = await this.context.defaultDB().update({table, where: where.join(" AND "), set});

    // @ts-ignore
    abap.builtin.sy.get().subrc.set(subrc);
    // @ts-ignore
    abap.builtin.sy.get().dbcnt.set(dbcnt);
    return subrc;
  }
}