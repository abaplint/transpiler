import {DB} from "@abaplint/runtime";
import * as pg from "pg";

export class PostgresDatabaseClient implements DB.DatabaseClient {
  public readonly name = "postgres";
  private readonly database;
  private readonly trace: boolean | undefined;
  private pool: pg.Pool | undefined;

  public constructor(input: {database: string, trace?: boolean}) {
    this.database = input.database;
    this.trace = input.trace;
  }

  public async connect() {
    this.pool = new pg.Pool({
      user: "postgres",
      host: "localhost",
      database: this.database,
      password: "postgres",
      port: 5432,
    });
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

  public async delete(_options: DB.DeleteDatabaseOptions): Promise<{ subrc: number; dbcnt: number; }> {
    throw new Error("Method not implemented.");
  }

  public async update(_options: DB.UpdateDatabaseOptions): Promise<{ subrc: number; dbcnt: number; }> {
    throw new Error("Method not implemented.");
  }

  public async insert(_options: DB.InsertDatabaseOptions): Promise<{ subrc: number; dbcnt: number; }> {
    throw new Error("Method not implemented.");
  }

  public async select(options: DB.SelectDatabaseOptions): Promise<DB.SelectDatabaseResult> {

    options.select = options.select.replace(/ UP TO (\d+) ROWS(.*)/i, "$2 LIMIT $1");

    if (this.trace === true) {
      console.log(options.select);
    }

    const res = await this.pool!.query(options.select);

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

}