import initSqlJs, {Database} from "sql.js";
import {DatabaseClient} from "./db";

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

  public exec(sql: string) {
    return this.sqlite!.exec(sql);
  }

  public prepare(sql: string) {
    return this.sqlite!.prepare(sql);
  }

  public delete(table: string, where: string): {subrc: number, dbcnt: number} {
    const sql = `DELETE FROM ${table} WHERE ${where}`;

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

}