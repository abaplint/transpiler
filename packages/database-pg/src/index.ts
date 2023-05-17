import {DB} from "@abaplint/runtime";
import * as pg from "pg";

export class PostgresDatabaseClient implements DB.DatabaseClient {
  public readonly name = "postgres";
  private pool: pg.Pool;

  public async connect() {
    this.pool = new pg.Pool({
      user: "username",
      host: "localhost",
      database: process.env.PGDATABASE,
      password: process.env.PGPASSWORD,
      port: 8080,
    });
  }

  public async disconnect(): Promise<void> {
    throw new Error("Method not implemented.");
  }

  public async execute(_sql: string | string[]): Promise<void> {
    throw new Error("Method not implemented.");
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

  public async select(_options: DB.SelectDatabaseOptions): Promise<DB.SelectDatabaseResult> {
    await this.pool.query("sdfsdf");
    throw new Error("Method not implemented.");
  }

}