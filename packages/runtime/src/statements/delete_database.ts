import {Context} from "../context";
import {buildDbTableName} from "../prefix";
import {FieldSymbol, Structure, Table} from "../types";
import {ICharacter} from "../types/_character";
import {toValue} from "./insert_database";

export interface IDeleteDatabaseOptions {
  from?: Structure | FieldSymbol,
  where?: string,
  table?: Table | FieldSymbol,
}

export async function deleteDatabase(table: string | ICharacter, options: IDeleteDatabaseOptions, context: Context) {
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
      await deleteDatabase(table, {from: row}, context);
    }
  } else if (options.from) {
    let where: string[] | string = [];

    const structure = options.from.get();
    for (const k of Object.keys(structure)) {
      const str = `"${k.toLowerCase()}"` + " = " + toValue(structure[k].get());
      where.push(str);
    }
    where = where.join(" AND ");

    const {subrc, dbcnt} = await context.defaultDB().delete({
      table: buildDbTableName(table),
      where,
    });

      // @ts-ignore
    abap.builtin.sy.get().subrc.set(subrc);
      // @ts-ignore
    abap.builtin.sy.get().dbcnt.set(dbcnt);
  } else {
    const {subrc, dbcnt} = await context.defaultDB().delete({
      table: buildDbTableName(table),
      where: options.where || "",
    });

      // @ts-ignore
    abap.builtin.sy.get().subrc.set(subrc);
      // @ts-ignore
    abap.builtin.sy.get().dbcnt.set(dbcnt);
  }
}