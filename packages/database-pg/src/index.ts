import {DB} from "@abaplint/runtime";
import * as pg from "pg";

export class PostgresDatabaseClient implements DB.DatabaseClient {
  public readonly name = "postgres";
  private readonly database;
  private pool: pg.Pool | undefined;

  public constructor(database: string) {
    this.database = database;
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
    // todo
    console.dir(options);
    await this.pool?.query("sdfsdf");
    throw new Error("Method not implemented.");
  }

}