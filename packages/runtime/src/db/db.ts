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
  /** INTO CORRESPONDING FIELDS OF work area, components not in the field list are left untouched */
  corresponding?: boolean,
}

export type DatabaseValue = number | string | Uint8Array | null;
export type DatabaseRow = {[name: string]: DatabaseValue};
export type DatabaseRows = DatabaseRow[];

export interface SelectDatabaseResult {
  rows: DatabaseRows;
}

export type DatabaseCursorCallbacks = {
  fetchNextCursor: (packageSize: number) => Promise<SelectDatabaseResult>,
  closeCursor: () => Promise<void>,
};

export interface DatabaseClient {
  /*** the type/name/identifier of the database */
  name: string;

  connect(): Promise<void>;
  disconnect(): Promise<void>;

  /*** execute any native SQL command */
  execute(sql: string | string[]): Promise<void>;

  // transaction handling, implementing the ABAP LUW,
  // delete()/update()/insert() implicitly begin a transaction if none is open,
  // so COMMIT WORK and ROLLBACK WORK are able to end it,
  // disconnecting performs an implicit commit
  /*** no-op if a transaction is already open */
  beginTransaction(): Promise<void>;
  /*** no-op if no transaction is open */
  commit(): Promise<void>;
  /*** no-op if no transaction is open */
  rollback(): Promise<void>;

  // operations, there is no modify(), it has been implemented using update() and insert() in the runtime
  delete(options: DeleteDatabaseOptions): Promise<{subrc: number, dbcnt: number}>;
  update(options: UpdateDatabaseOptions): Promise<{subrc: number, dbcnt: number}>;
  insert(options: InsertDatabaseOptions): Promise<{subrc: number, dbcnt: number}>;
  select(options: SelectDatabaseOptions): Promise<SelectDatabaseResult>;

  // cursors
  openCursor(options: SelectDatabaseOptions): Promise<DatabaseCursorCallbacks>;
}