import {DB} from "@abaplint/runtime";
import * as snowflake from "snowflake-sdk";

export class SnowflakeDatabaseClient implements DB.DatabaseClient {
  public readonly name = "snowflake";
  private connection: snowflake.Connection;
  private readonly config: snowflake.ConnectionOptions;
  private readonly trace: boolean | undefined;

  public constructor(input: snowflake.ConnectionOptions & {trace?: boolean}) {
    this.config = input;
    this.trace = input.trace;
  }

  public async connect(): Promise<void> {

    this.connection = snowflake.createConnection({
      ...this.config,
      clientSessionKeepAlive: true,
    });

    await new Promise((resolve, reject) =>
      this.connection.connectAsync((err, conn) => {
        err ? reject(err) : resolve(conn);
      })
    );
  }

  public async disconnect() {
    await new Promise((resolve, reject) =>
      this.connection.destroy((err, conn) => {
        err ? reject(err) : resolve(conn);
      })
    );
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
        if (s.trim() === "") {
          continue;
        }
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

  public async delete(options: DB.DeleteDatabaseOptions): Promise<{subrc: number, dbcnt: number}> {
    const sql = `DELETE FROM ${options.table} WHERE ${options.where}`;

    let subrc = 0;
    let dbcnt = 0;
    try {
      if (this.trace === true) {
        console.log(sql);
      }

      const res: any = await new Promise((resolve, _reject) =>
        this.connection.execute({
          sqlText: sql,
          complete: function (err, stmt, rows) {
            if (err) {
            // for now, show the error and return zero results,
              console.dir(stmt.getSqlText());
              console.dir(err.message);
              subrc = 4;
              resolve([]);
            } else {
              resolve(rows);
            }
          }}));
      dbcnt = res[0]["number of rows deleted"];
      if (dbcnt === 0) {
        subrc = 4;
      }
    } catch (error) {
      subrc = 4;
    }

    return {subrc, dbcnt};
  }

  public async update(options: DB.UpdateDatabaseOptions): Promise<{subrc: number, dbcnt: number}> {
    const sql = `UPDATE ${options.table} SET ${options.set.join(", ")} WHERE ${options.where}`;

    let subrc = 0;
    let dbcnt = 0;
    try {
      if (this.trace === true) {
        console.log(sql);
      }

      const res: any = await new Promise((resolve, _reject) =>
        this.connection.execute({
          sqlText: sql,
          complete: function (err, stmt, rows) {
            if (err) {
            // for now, show the error and return zero results,
              console.dir(stmt.getSqlText());
              console.dir(err.message);
              subrc = 4;
              resolve([]);
            } else {
              resolve(rows);
            }
          }}));

      dbcnt = res[0]["number of rows updated"];
      if (dbcnt === 0) {
        subrc = 4;
      }
    } catch (error) {
      subrc = 4;
    }

    return {subrc, dbcnt};
  }

  public async insert(options: DB.InsertDatabaseOptions): Promise<{subrc: number, dbcnt: number}> {
    const sql = `INSERT INTO ${options.table} (${options.columns.map(c => "\"" + c + "\"").join(",")}) VALUES (${options.values.join(",")})`;

    let subrc = 0;
    let dbcnt = 0;
    try {
      if (this.trace === true) {
        console.log(sql);
      }

      await new Promise((resolve, _reject) =>
        this.connection.execute({
          sqlText: sql,
          complete: function (err, stmt, rows) {
            if (err) {
            // for now, show the error and return zero results,
              console.dir(stmt.getSqlText());
              console.dir(err.message);
              subrc = 4;
              resolve([]);
            } else {
              resolve(rows);
            }
          }}));

      dbcnt = 1;
    } catch (error) {
// note: snowflake does not enforce PRIMARY KEY
      subrc = 4;
    }
    return {subrc, dbcnt};
  }

  public async select(options: DB.SelectDatabaseOptions): Promise<DB.SelectDatabaseResult> {
    options.select = options.select.replace(/ UP TO (\d+) ROWS(.*)/i, "$2 LIMIT $1");
    if (options.primaryKey) {
      options.select = options.select.replace(/ ORDER BY PRIMARY KEY/i, " ORDER BY " + options.primaryKey.map(e => `"${e}"`).join(", "));
    } else {
      options.select = options.select.replace(/ ORDER BY PRIMARY KEY/i, "");
    }
    options.select = options.select.replace(/ ASCENDING/ig, " ASC");
    options.select = options.select.replace(/ DESCENDING/ig, " DESC");
    options.select = options.select.replace(/~/g, ".");

    if (this.trace === true) {
      console.log(options.select);
    }

    const rows = await new Promise((resolve, _reject) =>
      this.connection.execute({
        sqlText: options.select,
        complete: function (err, stmt, rows) {
          if (err) {
            // for now, show the error and return zero results,
            console.dir(stmt.getSqlText());
            console.dir(err.message);
            resolve([]);
          } else {
            resolve(rows);
          }
        }}));

    return {rows: rows as any};
  }

  public openCursor(): Promise<{ cursor: number; }> {
    throw new Error("snowflake-openCursor not implemented.");
  }

  public fetchNextCursor(_cursor: number): Promise<{ subrc: number; dbcnt: number; }> {
    throw new Error("snowflake-fetchCursor not implemented.");
  }

  public closeCursor(_cursor: number): Promise<void> {
    throw new Error("snowflake-closeCursor not implemented.");
  }
}