import initSqlJs, {Database} from "sql.js";
import {DatabaseClient} from "./db";

export class SQLiteDatabaseClient implements DatabaseClient {
  private sqlite: Database | undefined = undefined;

  public async connect() {
    const SQL = await initSqlJs();
    this.sqlite = new SQL.Database();
  }

  public async disconnect() {
    return;
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

}