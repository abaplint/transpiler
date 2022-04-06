import initSqlJs, {Database, QueryExecResult} from "sql.js";
import * as DB from "./db";

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

  public async execute(sql: string): Promise<void> {
    if (sql !== "") {
      this.sqlite!.run(sql);
    }
  }

  public delete(options: DB.DeleteDatabaseOptions): {subrc: number, dbcnt: number} {
    const sql = `DELETE FROM ${options.table} WHERE ${options.where}`;

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

  public update(options: DB.UpdateDatabaseOptions): {subrc: number, dbcnt: number} {
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

  public insert(options: DB.InsertDatabaseOptions): {subrc: number, dbcnt: number} {
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

  public select(options: DB.SelectDatabaseOptions): DB.SelectDatabaseResult {
    let res: undefined | QueryExecResult[] = undefined;
    try {
      res = this.sqlite!.exec(options.select);
    } catch (error) {
      // @ts-ignore
      if (abap.Classes["CX_SY_DYNAMIC_OSQL_SEMANTICS"] !== undefined) {
        // @ts-ignore
        throw new abap.Classes["CX_SY_DYNAMIC_OSQL_SEMANTICS"]();
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