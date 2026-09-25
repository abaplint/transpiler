import {Context} from "../context";
import {buildDbTableName} from "../prefix";
import {FieldSymbol, Structure, Table} from "../types";
import {ICharacter} from "../types/_character";
import {toValue} from "./insert_database";
import {ABAP} from "..";

declare const abap: ABAP;

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
    // one DELETE per row; sy-subrc is 4 when any row was not found,
    // sy-dbcnt counts the rows deleted
    let subrc = 0;
    let dbcnt = 0;
    for (const row of options.table.array()) {
      await deleteDatabase(table, {from: row}, context);
      if (abap.builtin.sy.get().subrc.get() !== 0) {
        subrc = 4;
      }
      dbcnt += abap.builtin.sy.get().dbcnt.get();
    }
    abap.builtin.sy.get().subrc.set(subrc);
    abap.builtin.sy.get().dbcnt.set(dbcnt);
  } else if (options.from) {
    let where: string[] | string = [];

    // the row is found by its primary key, the other fields are not compared
    const keys: string[] | undefined = abap.DDIC[table.toUpperCase()]?.keyFields;
    const structure = options.from.get();
    for (const k of Object.keys(structure)) {
      if (keys !== undefined && keys.length > 0 && keys.includes(k.toUpperCase()) === false) {
        continue;
      }
      const str = `"${k.toLowerCase()}"` + " = " + toValue(structure[k].get());
      where.push(str);
    }
    where = where.join(" AND ");

    const {subrc, dbcnt} = await context.defaultDB().delete({
      table: buildDbTableName(table),
      where,
    });

    abap.builtin.sy.get().subrc.set(subrc);
    abap.builtin.sy.get().dbcnt.set(dbcnt);
  } else {
    const {subrc, dbcnt} = await context.defaultDB().delete({
      table: buildDbTableName(table),
      where: options.where || "",
    });

    abap.builtin.sy.get().subrc.set(subrc);
    abap.builtin.sy.get().dbcnt.set(dbcnt);
  }
}