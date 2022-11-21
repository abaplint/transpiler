import initSqlJs, {Database, QueryExecResult} from "sql.js";
import {DB} from "@abaplint/runtime";

export class SQLiteDatabaseClient implements DB.DatabaseClient {
  public readonly name = "sqlite";
  private sqlite: Database | undefined = undefined;

  public async connect() {
    const SQL = await initSqlJs();
    this.sqlite = new SQL.Database();
  }

  public async disconnect() {
    this.sqlite!.close();
    this.sqlite = undefined;
  }

  public async execute(sql: string | string[]): Promise<void> {
    if (typeof sql === "string") {
      if (sql === "") {
        return;
      }
      this.sqlite!.run(sql);
    } else {
      for (const s of sql) {
        await this.execute(s);
      }
    }
  }

  public async beginTransaction() {
    return; // todo
  }

  public async commit() {
    return; // todo
  }

  public async rollback() {
    return; // todo
  }

  public async delete(options: DB.DeleteDatabaseOptions) {
    const sql = `DELETE FROM ${options.table} WHERE ${options.where}`;

    let subrc = 0;
    let dbcnt = 0;
    try {
      this.sqlite!.exec(sql);

// https://www.sqlite.org/c3ref/changes.html
      const chg = this.sqlite!.exec("SELECT changes()");
      dbcnt = chg[0]["values"][0][0] as number;
      if (dbcnt === 0) {
        subrc = 4;
      }
    } catch (error) {
      subrc = 4;
    }

    return {subrc, dbcnt};
  }

  public async update(options: DB.UpdateDatabaseOptions) {
    const sql = `UPDATE ${options.table} SET ${options.set.join(", ")} WHERE ${options.where}`;

    let subrc = 0;
    let dbcnt = 0;
    try {
      const res = this.sqlite!.exec(sql);
      dbcnt = res.length;
    } catch (error) {
      subrc = 4;
    }

    return {subrc, dbcnt};
  }

  public async insert(options: DB.InsertDatabaseOptions) {
    const sql = `INSERT INTO ${options.table} (${options.columns.join(",")}) VALUES (${options.values.join(",")})`;

    let subrc = 0;
    let dbcnt = 0;
    try {
      const res = this.sqlite!.exec(sql);
      dbcnt = res.length;
    } catch (error) {
      // eg "UNIQUE constraint failed" errors
      subrc = 4;
    }
    return {subrc, dbcnt};
  }

  // // https://www.sqlite.org/lang_select.html
  public async select(options: DB.SelectDatabaseOptions) {
    let res: undefined | QueryExecResult[] = undefined;
    try {
      options.select = options.select.replace(/ UP TO (\d+) ROWS(.*)/i, "$2 LIMIT $1");
      // workaround to escape namespaces, this will need more work
      options.select = options.select.replace(/ FROM (\/\w+\/\w+)/i, " FROM '$1' ");
      if (options.primaryKey) {
        options.select = options.select.replace(/ ORDER BY PRIMARY KEY/i, " ORDER BY " + options.primaryKey.join(", "));
      } else {
        options.select = options.select.replace(/ ORDER BY PRIMARY KEY/i, "");
      }
      options.select = options.select.replace(/~/g, ".");
//      console.dir(options.select);
      res = this.sqlite!.exec(options.select);
    } catch (error) {
      // @ts-ignore
      if (abap.Classes["CX_SY_DYNAMIC_OSQL_SEMANTICS"] !== undefined) {
        // @ts-ignore
        throw await new abap.Classes["CX_SY_DYNAMIC_OSQL_SEMANTICS"]().constructor_({sqlmsg: error.message || ""});
      }
      throw error;
    }

    const rows = this.convert(res);

    return {rows: rows};
  }

  private convert(res: QueryExecResult[]): DB.DatabaseRows {
    if (res === undefined || res.length === 0) {
      return [];
    }

    const rows: DB.DatabaseRows = [];
    for (const sqliteRow of res[0].values) {
      const row: DB.DatabaseRow = {};
      let i = 0;
      for (const columnName of res[0].columns) {
        row[columnName] = sqliteRow[i];
        i++;
      }
      rows.push(row);
    }
    return rows;
  }
}