import {ABAP, DB} from "@abaplint/runtime";
import {DuckDBConnection, DuckDBInstance} from "@duckdb/node-api";

declare const abap: ABAP;

/** ABAP compares CHAR values ignoring trailing blanks and the runtime pads its
    literals to the field length; DuckDB VARCHAR keeps what it gets and compares
    the blanks, so literals are trimmed on the way in */
function trimLiterals(sql: string): string {
  return sql.replace(/'((?:[^']|'')*)'/g, (_m, inner: string) => "'" + inner.replace(/ +$/, "") + "'");
}

/** the runtime wants number | string | Uint8Array | null */
function plain(value: unknown): DB.DatabaseValue {
  if (typeof value === "bigint") {
    return Number(value);
  }
  if (value === null || value === undefined) {
    return null;
  }
  if (value instanceof Uint8Array) {
    return value;
  }
  if (typeof value === "object") {
    // DECIMAL, DATE, TIMESTAMP etc. come back as value objects, take their textual form
    return String(value);
  }
  return value as DB.DatabaseValue;
}

/** the transpiler writes its DDL in the PostgreSQL flavour, DuckDB has no fixed-width
    character type, so NCHAR(n) becomes VARCHAR(n) */
export function duckdbSchema(schemas: string[]): string[] {
  return schemas.map(s => s.replace(/\bNCHAR\((\d+)\)/g, "VARCHAR($1)"));
}

/** seed rows are written for SQLite, which accepts single quoted column names:
    INSERT INTO reposrc ('PROGNAME', 'DATA') becomes ("progname", "data") */
export function duckdbInserts(inserts: string[]): string[] {
  return inserts.map(s => s.replace(/^(\s*INSERT INTO \S+ \()([^)]*)\)/i, (_m, head: string, cols: string) =>
    head + cols.replace(/'([A-Za-z_0-9]+)'/g, (_mm, c: string) => "\"" + c.toLowerCase() + "\"") + ")"));
}

export class DuckDBDatabaseClient implements DB.DatabaseClient {
  public readonly name = "duckdb";
  private readonly path: string;
  private readonly trace: boolean;
  private instance: DuckDBInstance | undefined;
  private connection: DuckDBConnection | undefined;
  private inTransaction = false;
  /** successful modifying statements of the open LUW, replayed when a failed
      statement aborts the DuckDB transaction (savepoint emulation) */
  private luw: string[] = [];

  /**
   * @param input.path Database file, ":memory:" (the default) keeps nothing between runs
   * @param input.trace If true, all SQL statements are printed to the console
   */
  public constructor(input?: {path?: string, trace?: boolean}) {
    this.path = input?.path ?? ":memory:";
    this.trace = input?.trace === true;
  }

  public async connect(): Promise<void> {
    this.instance = await DuckDBInstance.create(this.path);
    this.connection = await this.instance.connect();

    if (typeof abap !== "undefined" && abap?.context?.databaseConnections
        && abap.context.databaseConnections["DEFAULT"] === this) {
      abap.builtin.sy.get().dbsys?.set(this.name);
    }
  }

  public async disconnect(): Promise<void> {
    try {
      // ending the session performs an implicit commit
      await this.commit();
    } finally {
      this.connection?.closeSync();
      this.instance?.closeSync();
      this.connection = undefined;
      this.instance = undefined;
    }
  }

  private conn(): DuckDBConnection {
    if (this.connection === undefined) {
      throw new Error("DuckDB: Database connection not established");
    }
    return this.connection;
  }

  /** execute any native SQL, the DDL and seed rows the transpiler emits for PostgreSQL/SQLite are accepted as is */
  public async execute(sql: string | string[]): Promise<void> {
    if (typeof sql !== "string") {
      for (const s of sql) {
        await this.execute(s);
      }
      return;
    }
    if (sql === "") {
      return;
    }
    if (/^\s*CREATE TABLE/i.test(sql)) {
      sql = duckdbSchema([sql])[0];
    } else if (/^\s*INSERT/i.test(sql)) {
      sql = trimLiterals(duckdbInserts([sql])[0]);
    }
    if (this.trace === true) {
      console.log(sql);
    }
    await this.conn().run(sql);
  }

  public async beginTransaction(): Promise<void> {
    if (this.inTransaction === true) {
      return;
    }
    await this.conn().run("BEGIN TRANSACTION");
    this.inTransaction = true;
    this.luw = [];
  }

  public async commit(): Promise<void> {
    await this.endTransaction("COMMIT");
  }

  public async rollback(): Promise<void> {
    await this.endTransaction("ROLLBACK");
  }

  private async endTransaction(sql: "COMMIT" | "ROLLBACK"): Promise<void> {
    if (this.inTransaction === false) {
      return;
    }
    this.inTransaction = false;
    this.luw = [];
    await this.conn().run(sql);
  }

  /** DuckDB aborts the full transaction if a statement fails and has no savepoints,
      so the successful statements of the LUW are replayed into a fresh transaction,
      allowing the LUW to continue after eg. duplicate keys */
  private async modifying(sql: string): Promise<number> {
    await this.beginTransaction();
    if (this.trace === true) {
      console.log(sql);
    }
    let rows: Record<string, unknown>[];
    try {
      rows = (await this.conn().runAndReadAll(sql)).getRowObjects();
    } catch (error) {
      await this.conn().run("ROLLBACK");
      await this.conn().run("BEGIN TRANSACTION");
      for (const replay of this.luw) {
        await this.conn().run(replay);
      }
      throw error;
    }
    this.luw.push(sql);
    // the affected row count is a single-row result
    return rows.length > 0 ? Number(Object.values(rows[0])[0] ?? 0) : 0;
  }

  public async delete(options: DB.DeleteDatabaseOptions): Promise<{subrc: number, dbcnt: number}> {
    let sql = `DELETE FROM ${options.table}`;
    if (options.where !== "") {
      sql += ` WHERE ${options.where}`;
    }
    try {
      const dbcnt = await this.modifying(trimLiterals(sql));
      return {subrc: dbcnt === 0 ? 4 : 0, dbcnt};
    } catch (error) {
      if (this.trace === true) {
        console.dir(error);
      }
      return {subrc: 4, dbcnt: 0};
    }
  }

  public async update(options: DB.UpdateDatabaseOptions): Promise<{subrc: number, dbcnt: number}> {
    const sql = `UPDATE ${options.table} SET ${options.set.join(", ")} WHERE ${options.where}`;
    try {
      const dbcnt = await this.modifying(trimLiterals(sql));
      return {subrc: dbcnt === 0 ? 4 : 0, dbcnt};
    } catch (error) {
      if (this.trace === true) {
        console.dir(error);
      }
      return {subrc: 4, dbcnt: 0};
    }
  }

  public async insert(options: DB.InsertDatabaseOptions): Promise<{subrc: number, dbcnt: number}> {
    const sql = `INSERT INTO ${options.table} (${options.columns.map(c => "\"" + c + "\"").join(",")}) VALUES (${options.values.join(",")})`;
    try {
      const dbcnt = await this.modifying(trimLiterals(sql));
      return {subrc: 0, dbcnt};
    } catch (error) {
      if (this.trace === true) {
        console.dir(error);
      }
      // eg. duplicate key errors
      return {subrc: 4, dbcnt: 0};
    }
  }

  private rewrite(options: DB.SelectDatabaseOptions): string {
    let sql = options.select.replace(/ UP TO (\d+) ROWS(.*)/i, "$2 LIMIT $1");
    if (options.primaryKey) {
      sql = sql.replace(/ ORDER BY PRIMARY KEY/i, " ORDER BY " + options.primaryKey.join(", "));
    } else {
      sql = sql.replace(/ ORDER BY PRIMARY KEY/i, "");
    }
    sql = sql.replace(/ ASCENDING/ig, " ASC");
    sql = sql.replace(/ DESCENDING/ig, " DESC");
    sql = sql.replace(/~/g, ".");
    sql = sql.replace(/ LIMIT 0/g, "");
    return trimLiterals(sql);
  }

  private async query(sql: string): Promise<DB.DatabaseRows> {
    if (this.trace === true) {
      console.log(sql);
    }
    let rows: Record<string, unknown>[];
    try {
      rows = (await this.conn().runAndReadAll(sql)).getRowObjects();
    } catch (error) {
      if (typeof abap !== "undefined" && abap.Classes["CX_SY_DYNAMIC_OSQL_SEMANTICS"] !== undefined) {
        throw await new abap.Classes["CX_SY_DYNAMIC_OSQL_SEMANTICS"]().constructor_({sqlmsg: error.message || ""});
      }
      throw error;
    }
    return rows.map(r => {
      const row: DB.DatabaseRow = {};
      for (const columnName in r) {
        row[columnName] = plain(r[columnName]);
      }
      return row;
    });
  }

  public async select(options: DB.SelectDatabaseOptions): Promise<DB.SelectDatabaseResult> {
    options.select = this.rewrite(options);
    return {rows: await this.query(options.select)};
  }

  /** a client-side slice over a full read */
  public async openCursor(options: DB.SelectDatabaseOptions): Promise<DB.DatabaseCursorCallbacks> {
    const rows = await this.query(this.rewrite(options));
    let offset = 0;
    return {
      fetchNextCursor: async (packageSize: number) => {
        const batch = rows.slice(offset, offset + packageSize);
        offset += batch.length;
        return {rows: batch};
      },
      closeCursor: async () => undefined,
    };
  }

}
