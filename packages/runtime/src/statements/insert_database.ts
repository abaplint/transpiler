import {Context} from "../context";
import {buildDbTableName} from "../prefix";
import {Structure, Table} from "../types";
import {ICharacter} from "../types/_character";

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
}

export async function insertDatabase(table: string | ICharacter, options: IInsertDatabaseOptions, context: Context) {
  const columns: string[] = [];
  const values: string[] = [];

  if (options.values === undefined && options.table === undefined) {
    throw "insertDatabase, wrong input";
  }

  if (options.table !== undefined) {
    let subrc = 0;
    let dbcnt = 0;
    for (const row of options.table.array()) {
      await insertDatabase(table, {values: row, connection: options.connection}, context);
      // @ts-ignore
      subrc = Math.max(subrc, abap.builtin.sy.get().subrc.get());
      // @ts-ignore
      dbcnt += abap.builtin.sy.get().dbcnt.get();
    }
    // @ts-ignore
    abap.builtin.sy.get().subrc.set(subrc);
    // @ts-ignore
    abap.builtin.sy.get().dbcnt.set(dbcnt);
    return;
  }

  const structure = options.values!.get();
  for (const k of Object.keys(structure)) {
    columns.push(k);

    const value = structure[k].get();
    if (value instanceof Structure) {
      // then its a group, ignore
      continue;
    }
    values.push(toValue(value));
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

    // @ts-ignore
  abap.builtin.sy.get().subrc.set(subrc);
    // @ts-ignore
  abap.builtin.sy.get().dbcnt.set(dbcnt);
  return subrc;
}