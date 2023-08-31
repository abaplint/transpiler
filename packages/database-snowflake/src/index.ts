import {DB} from "@abaplint/runtime";
import * as snowflake from "snowflake-sdk";

export class SnowflakeDatabaseClient implements DB.DatabaseClient {
  public readonly name = "snowflake";
  private connection: snowflake.Connection;

  public constructor(_input?: {trace?: boolean}) {
    // todo
  }

  public async connect(input?: snowflake.ConnectionOptions): Promise<void> {
    if (input === undefined) {
      throw "supply connection details";
    }

    this.connection = snowflake.createConnection({
      ...input,
      clientSessionKeepAlive: true,
    });

    await new Promise((resolve, reject) =>
      this.connection.connect((err, conn) => {
        err ? reject(err) : resolve(conn);
      })
    );
  }

  public async disconnect() {
    throw "todo";
  }

  public async execute(sql: string | string[]): Promise<void> {
    if (typeof sql === "string") {
      if (sql === "") {
        return;
      }
      await new Promise((resolve, reject) =>
        this.connection.execute({
          sqlText: sql,
          complete: function (err, _stmt, rows) {
            err ? reject(err) : resolve(rows);
          }}));
    } else {
      for (const s of sql) {
        await new Promise((resolve, reject) =>
          this.connection.execute({
            sqlText: s,
            complete: function (err, _stmt, rows) {
              err ? reject(err) : resolve(rows);
            }}));
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

  public async delete(_options: DB.DeleteDatabaseOptions): Promise<{subrc: number, dbcnt: number}> {
    throw "todo";
  }

  public async update(_options: DB.UpdateDatabaseOptions): Promise<{subrc: number, dbcnt: number}> {
    throw "todo";
  }

  public async insert(_options: DB.InsertDatabaseOptions): Promise<{subrc: number, dbcnt: number}> {
    throw "todo";
  }

  public async select(_options: DB.SelectDatabaseOptions): Promise<DB.SelectDatabaseResult> {
    throw "todo";
  }
}