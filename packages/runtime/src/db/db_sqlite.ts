import initSqlJs, {Database} from "sql.js";
import {DatabaseClient, DeleteDatabaseOptions, InsertDatabaseOptions, SelectDatabaseOptions, SelectDatabaseResult, UpdateDatabaseOptions} from "./db";

export class SQLiteDatabaseClient implements DatabaseClient {
  private sqlite: Database | undefined = undefined;

  public async connect() {
    const SQL = await initSqlJs();
    this.sqlite = new SQL.Database();
  }

  public async disconnect() {
    this.sqlite!.close();
    this.sqlite = undefined;
  }

  public async initialize(sql?: string): Promise<void> {
    if (sql && sql !== "") {
      this.sqlite!.run(sql);
    }
  }

  public prepare(sql: string) {
    return this.sqlite!.prepare(sql);
  }

  public delete(options: DeleteDatabaseOptions): {subrc: number, dbcnt: number} {
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

  public update(options: UpdateDatabaseOptions): {subrc: number, dbcnt: number} {
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

  public insert(options: InsertDatabaseOptions): {subrc: number, dbcnt: number} {
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

  public select(options: SelectDatabaseOptions): SelectDatabaseResult {
    let res: undefined | any = undefined;
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
    return {result: res};
  }
}