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
  /** select statement, in ABAP SQL syntax */
  select: string,
  /** list of primary key fields, in lower case, if known */
  primaryKey?: string[],
}

export interface SelectRuntimeOptions {
  appending?: boolean,
}

export type DatabaseValue = number | string | Uint8Array | null;
export type DatabaseRow = {[name: string]: DatabaseValue};
export type DatabaseRows = DatabaseRow[];

export interface SelectDatabaseResult {
  rows: DatabaseRows;
}

export interface DatabaseClient {
  /*** the type/name/identifier of the database */
  name: string;

  connect(): Promise<void>;
  disconnect(): Promise<void>;

  /*** execute any native SQL command */
  execute(sql: string | string[]): Promise<void>;

  // transaction handling
  beginTransaction(): Promise<void>;
  commit(): Promise<void>;
  rollback(): Promise<void>;

  // operations, there is no modify(), it has been implemented using update() and insert() in the runtime
  delete(options: DeleteDatabaseOptions): Promise<{subrc: number, dbcnt: number}>;
  update(options: UpdateDatabaseOptions): Promise<{subrc: number, dbcnt: number}>;
  insert(options: InsertDatabaseOptions): Promise<{subrc: number, dbcnt: number}>;
  select(options: SelectDatabaseOptions): Promise<SelectDatabaseResult>;

  // cursors
  openCursor(options: SelectDatabaseOptions): Promise<{cursor: number}>;
  fetchNextCursor(cursor: number, packageSize: number): Promise<SelectDatabaseResult>;
  closeCursor(cursor: number): Promise<void>;
}