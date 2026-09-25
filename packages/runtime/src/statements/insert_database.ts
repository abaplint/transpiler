import {Context} from "../context";
import {buildDbTableName} from "../prefix";
import {String, Structure, Table} from "../types";
import {ICharacter} from "../types/_character";
import {ABAP} from "..";
import {throwErrorWithParameters} from "../throw_error";

declare const abap: ABAP;

export function toValue(value: any) {
  if (typeof value === "string") {
    // postgres requires ' for values
    return "'" + value.replace(/'/g, "''") + "'";
//    return '"' + value.replace(/"/g, "\"\"") + '"';
  } else {
    return value;
  }
}

export interface IInsertDatabaseOptions {
  values?: Structure,
  table?: Table,
  connection?: string,
  acceptingDuplicateKeys?: boolean,
}

export async function insertDatabase(table: string | ICharacter, options: IInsertDatabaseOptions, context: Context) {
  const columns: string[] = [];
  const values: string[] = [];

  if (options.values === undefined && options.table === undefined) {
    throw new Error("insertDatabase, wrong input");
  }

  if (options.table !== undefined) {
    const subrcBefore = abap.builtin.sy.get().subrc.get();
    const dbcntBefore = abap.builtin.sy.get().dbcnt.get();
    let subrc = 0;
    let dbcnt = 0;
    for (const row of options.table.array()) {
      await insertDatabase(table, {values: row, connection: options.connection}, context);
      subrc = Math.max(subrc, abap.builtin.sy.get().subrc.get());
      dbcnt += abap.builtin.sy.get().dbcnt.get();
    }
    if (subrc !== 0 && options.acceptingDuplicateKeys !== true) {
      // without ACCEPTING DUPLICATE KEYS a row that cannot be inserted raises
      // CX_SY_OPEN_SQL_DB; the other rows stay inserted and sy-subrc and
      // sy-dbcnt keep the values they had before the statement
      abap.builtin.sy.get().subrc.set(subrcBefore);
      abap.builtin.sy.get().dbcnt.set(dbcntBefore);
      await throwErrorWithParameters("CX_SY_OPEN_SQL_DB", {sqlmsg: new String().set(`INSERT ${typeof table === "string" ? table : table.get().trimEnd()} FROM TABLE: a row could not be inserted (duplicate key?)`)});
    }
    abap.builtin.sy.get().subrc.set(subrc);
    abap.builtin.sy.get().dbcnt.set(dbcnt);
    return;
  }

  const structure = options.values!.get();
  for (const k of Object.keys(structure)) {
    const field = structure[k];
    if (field instanceof Structure) {
      // then its a group, ignore
      continue;
    }

    columns.push(k);
    values.push(toValue(field.get()));
  }

  if (typeof table !== "string") {
    table = table.get().trimEnd().toLowerCase();
  }

  let db = context.defaultDB();
  if (options.connection) {
    db = context.databaseConnections[options.connection];
  }
  const {subrc, dbcnt} = await db.insert({
    table: buildDbTableName(table),
    columns,
    values,
  });

  abap.builtin.sy.get().subrc.set(subrc);
  abap.builtin.sy.get().dbcnt.set(dbcnt);
  return subrc;
}