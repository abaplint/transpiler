import {QueryExecResult} from "sql.js";

// todo defined inputs and returns?

export interface DeleteDatabaseOptions {
  table: string,
  where: string,
}

export interface UpdateDatabaseOptions {
  table: string,
  where: string,
  set: string[],
}

export interface InsertDatabaseOptions {
  table: string,
  columns: string[],
  values: string[],
}

export interface SelectDatabaseOptions {
  select: string,
}

export interface SelectDatabaseResult {
  result: QueryExecResult[]
}

export interface DatabaseClient {
  initialize(sql?: string): Promise<void>;
  connect(): Promise<void>;
  disconnect(): Promise<void>;

  delete(options: DeleteDatabaseOptions): {subrc: number, dbcnt: number};
  update(options: UpdateDatabaseOptions): {subrc: number, dbcnt: number};
  insert(options: InsertDatabaseOptions): {subrc: number, dbcnt: number};
  select(options: SelectDatabaseOptions): SelectDatabaseResult;
}