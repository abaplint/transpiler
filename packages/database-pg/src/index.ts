import {DB} from "@abaplint/runtime";
import * as pg from "pg";

export type ConnectionSettings = {
  user: string,
  host: string,
  database: string,
  password: string,
  port: number,
};

export class PostgresDatabaseClient implements DB.DatabaseClient {
  public readonly name = "postgres";
  private readonly config: pg.PoolConfig;
  private readonly trace: boolean | undefined;
  private pool: pg.Pool | undefined;

  public constructor(input: ConnectionSettings & {trace?: boolean}) {
    this.config = input;
    this.trace = input.trace;
  }

  public async connect(pool?: pg.Pool) {
    if (pool) {
      this.pool = pool;
    } else {
      this.pool = new pg.Pool({
        user: this.config.user,
        host: this.config.host,
        database: this.config.database,
        password: this.config.password,
        port: 5432,
      });
    }
    // cleanup after use
    this.config.password = "";

    // @ts-ignore
    if (abap?.context?.databaseConnections && abap.context.databaseConnections["DEFAULT"] === this) {
      // @ts-ignore
      abap.builtin.sy.get().dbsys?.set(this.name);
    }
  }

  public async disconnect(): Promise<void> {
    await this.pool?.end();
    this.pool = undefined;
  }

  public async execute(sql: string | string[]): Promise<void> {
    if (typeof sql === "string") {
      if (sql === "") {
        return;
      }
      await this.pool!.query(sql);
    } else {
      for (const s of sql) {
        await this.execute(s);
      }
    }
  }

  public async beginTransaction(): Promise<void> {
    throw new Error("Method not implemented.");
  }

  public async commit(): Promise<void> {
    throw new Error("Method not implemented.");
  }

  public async rollback(): Promise<void> {
    throw new Error("Method not implemented.");
  }

  public async delete(options: DB.DeleteDatabaseOptions): Promise<{ subrc: number; dbcnt: number; }> {
    const sql = `DELETE FROM ${options.table} WHERE ${options.where}`;

    let subrc = 0;
    let dbcnt = 0;
    try {
      if (this.trace === true) {
        console.log(sql);
      }

      const res = await this.pool!.query(sql);
      dbcnt = res.rowCount;
      if (dbcnt === 0) {
        subrc = 4;
      }
    } catch (error) {
      subrc = 4;
    }

    return {subrc, dbcnt};
  }

  public async update(options: DB.UpdateDatabaseOptions): Promise<{ subrc: number; dbcnt: number; }> {
    const sql = `UPDATE ${options.table} SET ${options.set.join(", ")} WHERE ${options.where}`;

    let subrc = 0;
    let dbcnt = 0;
    try {
      if (this.trace === true) {
        console.log(sql);
      }

      const res = await this.pool!.query(sql);
      dbcnt = res.rowCount;
      if (dbcnt === 0) {
        subrc = 4;
      }
    } catch (error) {
      subrc = 4;
    }

    return {subrc, dbcnt};
  }

  public async insert(options: DB.InsertDatabaseOptions): Promise<{ subrc: number; dbcnt: number; }> {
    const sql = `INSERT INTO ${options.table} (${options.columns.map(c => "\"" + c + "\"").join(",")}) VALUES (${options.values.join(",")})`;

    let subrc = 0;
    let dbcnt = 0;
    try {
      if (this.trace === true) {
        console.log(sql);
      }

      const res = await this.pool!.query(sql);
      dbcnt = res.rowCount;
    } catch (error) {
      // eg "UNIQUE constraint failed" errors
      subrc = 4;
    }
    return {subrc, dbcnt};
  }

  public async select(options: DB.SelectDatabaseOptions): Promise<DB.SelectDatabaseResult> {
    let res: undefined | pg.QueryResult<any> = undefined;

    options.select = options.select.replace(/ UP TO (\d+) ROWS(.*)/i, "$2 LIMIT $1");
    if (options.primaryKey) {
      options.select = options.select.replace(/ ORDER BY PRIMARY KEY/i, " ORDER BY " + options.primaryKey.join(", "));
    } else {
      options.select = options.select.replace(/ ORDER BY PRIMARY KEY/i, "");
    }
    options.select = options.select.replace(/ ASCENDING/ig, " ASC");
    options.select = options.select.replace(/ DESCENDING/ig, " DESC");
    options.select = options.select.replace(/~/g, ".");

    if (this.trace === true) {
      console.log(options.select);
    }

    try {
      res = await this.pool!.query(options.select);
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

  private convert(res: pg.QueryResult<any>): DB.DatabaseRows {
    if (res === undefined || res.rows.length === 0) {
      return [];
    }

    const rows: DB.DatabaseRows = [];
    for (const pgRow of res.rows) {
      const row: DB.DatabaseRow = {};
      for (const columnName in pgRow) {
        row[columnName] = pgRow[columnName];
      }
      rows.push(row);
    }
    return rows;
  }

  public openCursor(): Promise<{ cursor: number; }> {
    throw new Error("pg-openCursor not implemented.");
  }

  public fetchCursor(_cursor: number): Promise<{ subrc: number; dbcnt: number; }> {
    throw new Error("pg-fetchCursor not implemented.");
  }

  public closeCursor(_cursor: number): Promise<void> {
    throw new Error("pg-closeCursor not implemented.");
  }

}