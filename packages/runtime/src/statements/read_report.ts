import {String, Table} from "../types";
import {Context} from "../context";
import {ICharacter} from "../types/_character";
import {ABAP} from "..";

declare const abap: ABAP;

interface IReadReportOptions {
  state?: ICharacter,
  into?: Table,
}

function escape(value: string): string {
  return value.replace(/'/g, "''");
}

export async function readReport(name: ICharacter, options: IReadReportOptions, context: Context) {
  const progname = name.get().trimEnd().toUpperCase().padEnd(40, " ");
  const select = `SELECT "data" FROM ${abap.buildDbTableName("reposrc")} WHERE "progname" = '${escape(progname)}' UP TO 1 ROWS`;
  const {rows} = await context.defaultDB().select({select, primaryKey: ["progname"]});

  if (rows.length === 0) {
    options.into?.clear();
    abap.builtin.sy.get().subrc.set(4);
    return;
  }

  if (options.into) {
    options.into.clear();
    const data = rows[0]["data"]?.toString() || "";
    for (const line of data.split(/\r?\n/)) {
      options.into.append(new String().set(line));
    }
  }

  abap.builtin.sy.get().subrc.set(0);
}
