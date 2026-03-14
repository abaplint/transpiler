import {Context} from "../context";
import {buildDbTableName} from "../prefix";
import {FieldSymbol, String, Structure, Table} from "../types";
import {ICharacter} from "../types/_character";
import {toValue} from "./insert_database";
import {ABAP} from "..";
import {throwErrorWithParameters} from "../throw_error";

declare const abap: ABAP;

export interface IUpdateDatabaseOptions {
  from?: Structure | FieldSymbol,
  table?: Table | FieldSymbol,
  set?: string[],
  where?: string,
}

export async function updateDatabase(table: string | ICharacter, options: IUpdateDatabaseOptions, context: Context) {
  if (options.table instanceof FieldSymbol) {
    options.table = options.table.getPointer() as Table;
  }
  if (options.from instanceof FieldSymbol) {
    options.from = options.from.getPointer() as Structure;
  }

  if (typeof table !== "string") {
    table = table.get();
  }

  const tabl = abap.DDIC[table.toUpperCase()];
  if (tabl === undefined) {
    await throwErrorWithParameters("CX_SY_DYNAMIC_OSQL_SEMANTICS", {sqlmsg: new String().set(`Table ${table} not found`)});
  }

  const keys: string[] = tabl.keyFields;
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
    console.dir(table);
    console.dir(options);
    throw new Error("updateDatabase, todo");
  }

  const {subrc, dbcnt} = await context.defaultDB().update({
    table: buildDbTableName(table),
    where: where.join(" AND "),
    set,
  });

  abap.builtin.sy.get().subrc.set(subrc);
  abap.builtin.sy.get().dbcnt.set(dbcnt);
  return subrc;
}