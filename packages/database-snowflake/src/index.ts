import {DB} from "@abaplint/runtime";

export class SnowflakeDatabaseClient implements DB.DatabaseClient {
  public readonly name = "snowflake";

  public constructor(_input?: {trace?: boolean}) {
    // todo
  }

  public async connect(_data?: ArrayLike<number> | Buffer | null) {
    throw "todo";
  }

  public async disconnect() {
    throw "todo";
  }

  public async execute(_sql: string | string[]): Promise<void> {
    throw "todo";
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