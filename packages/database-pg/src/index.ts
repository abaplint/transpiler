import {DB} from "@abaplint/runtime";

export class PostgresDatabaseClient implements DB.DatabaseClient {
  public readonly name = "postgres";

  public connect(): Promise<void> {
    throw new Error("Method not implemented.");
  }

  public disconnect(): Promise<void> {
    throw new Error("Method not implemented.");
  }

  public execute(_sql: string | string[]): Promise<void> {
    throw new Error("Method not implemented.");
  }

  public beginTransaction(): Promise<void> {
    throw new Error("Method not implemented.");
  }

  public commit(): Promise<void> {
    throw new Error("Method not implemented.");
  }

  public rollback(): Promise<void> {
    throw new Error("Method not implemented.");
  }

  public delete(_options: DB.DeleteDatabaseOptions): Promise<{ subrc: number; dbcnt: number; }> {
    throw new Error("Method not implemented.");
  }

  public update(_options: DB.UpdateDatabaseOptions): Promise<{ subrc: number; dbcnt: number; }> {
    throw new Error("Method not implemented.");
  }

  public insert(_options: DB.InsertDatabaseOptions): Promise<{ subrc: number; dbcnt: number; }> {
    throw new Error("Method not implemented.");
  }

  public select(_options: DB.SelectDatabaseOptions): Promise<DB.SelectDatabaseResult> {
    throw new Error("Method not implemented.");
  }

}