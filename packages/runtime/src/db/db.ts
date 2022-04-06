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

export type DatabaseValue = number | string | Uint8Array | null;
export type DatabaseRow = {[name: string]: DatabaseValue};
export type DatabaseRows = DatabaseRow[];

export interface SelectDatabaseResult {
  result: DatabaseRows;
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