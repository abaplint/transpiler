import {DB} from "@abaplint/runtime";
import * as pg from "pg";
import Cursor from "pg-cursor";

export type ConnectionSettings = {
  user: string,
  host: string,
  database: string,
  password: string,
  port: number,
};

export class PostgresDatabaseClient implements DB.DatabaseClient {
  public readonly name = "postgres";
  private readonly config: pg.PoolConfig;
  private readonly trace: boolean | undefined;
  private readonly lazy: boolean | undefined;
  private pool: pg.Pool | undefined;
  /** set while a transaction is open, all statements of the LUW must run on the same connection */
  private client: pg.PoolClient | undefined;
  /** set if a COMMIT failed, the changes of that LUW are lost and the client is unusable */
  private fatal: Error | undefined;

  /**
   * @param input Connection settings
   * @param input.trace If true, all SQL queries are printed to the console
   * @param input.lazy If true, the connection is not established until the first query
   */
  public constructor(input: ConnectionSettings & {trace?: boolean, lazy?: boolean}) {
    this.config = input;
    this.trace = input.trace;
    this.lazy = input.lazy;
  }

  public async connect(pool?: pg.Pool) {
    if (pool) {
      this.pool = pool;
    } else {
      this.pool = new pg.Pool({
        user: this.config.user,
        host: this.config.host,
        database: this.config.database,
        password: this.config.password,
        port: this.config.port,
      });
    }
    // cleanup after use
    this.config.password = "";

    // @ts-ignore
    if (global["abap"]) {
      // @ts-ignore
      if (abap?.context?.databaseConnections !== undefined && abap?.context.databaseConnections["DEFAULT"] === this) {
        // @ts-ignore
        abap.builtin.sy.get().dbsys?.set(this.name);
      }
    }
  }

  public async disconnect(): Promise<void> {
    try {
      // ending the session performs an implicit commit
      await this.commit();
    } finally {
      // release the pool even if the implicit commit threw, otherwise the
      // process cannot terminate and the application hangs instead of crashing
      await this.pool?.end();
      this.pool = undefined;
    }
  }

  public async execute(sql: string | string[]): Promise<void> {
    this.checkFatal();
    if (this.lazy === true && this.pool === undefined) {
      await this.connect();
    }

    if (typeof sql === "string") {
      if (sql === "") {
        return;
      }
      await this.query(sql);
    } else {
      for (const s of sql) {
        await this.execute(s);
      }
    }
  }

  /** runs on the transaction connection if a transaction is open, otherwise on the pool */
  private async query(sql: string): Promise<pg.QueryResult<any>> {
    if (this.client !== undefined) {
      return this.client.query(sql);
    }
    if (this.pool === undefined) {
      throw new Error("PG: Database connection not established");
    }
    return this.pool.query(sql);
  }

  public async beginTransaction(): Promise<void> {
    this.checkFatal();
    if (this.client !== undefined) {
      return;
    }
    if (this.lazy === true && this.pool === undefined) {
      await this.connect();
    }
    if (this.pool === undefined) {
      throw new Error("PG: Database connection not established");
    }
    const client = await this.pool.connect();
    try {
      await client.query("BEGIN");
      this.client = client;
    } catch (error) {
      client.release(error instanceof Error ? error : true);
      throw error;
    }
  }

  public async commit(): Promise<void> {
    await this.endTransaction("COMMIT");
  }

  public async rollback(): Promise<void> {
    await this.endTransaction("ROLLBACK");
  }

  private async endTransaction(sql: "COMMIT" | "ROLLBACK"): Promise<void> {
    const client = this.client;
    if (client === undefined) {
      return;
    }
    this.client = undefined;
    try {
      await client.query(sql);
      client.release();
    } catch (error) {
      client.release(error instanceof Error ? error : true);
      if (sql === "COMMIT") {
        // postgres has already rolled the transaction back, so the changes of this LUW
        // are gone. There is no way to complete the LUW, and silently carrying on would
        // lose the data, so poison the client and take the application down
        this.fatal = new Error("PG: COMMIT failed, the changes of the current LUW are lost: "
          + (error instanceof Error ? error.message : error), {cause: error});
      }
      throw this.fatal ?? error;
    }
  }

  /** a failed COMMIT is not recoverable, every further use of the client must crash
      rather than degrade into sy-subrc = 4 */
  private checkFatal(): void {
    if (this.fatal !== undefined) {
      throw this.fatal;
    }
  }

  /** postgres aborts the full transaction if a statement fails, so wrap modifying
      statements in a savepoint, allowing the LUW to continue after eg. duplicate keys */
  private async modifying(sql: string): Promise<pg.QueryResult<any>> {
    await this.beginTransaction();

    await this.client!.query("SAVEPOINT abap_stmt");
    try {
      const res = await this.client!.query(sql);
      await this.client!.query("RELEASE SAVEPOINT abap_stmt");
      return res;
    } catch (error) {
      await this.client!.query("ROLLBACK TO SAVEPOINT abap_stmt; RELEASE SAVEPOINT abap_stmt;");
      throw error;
    }
  }

  public async delete(options: DB.DeleteDatabaseOptions): Promise<{ subrc: number; dbcnt: number; }> {
    this.checkFatal();
    if (this.lazy === true && this.pool === undefined) {
      await this.connect();
    }

    let sql = `DELETE FROM ${options.table}`;
    if (options.where !== "") {
      sql += ` WHERE ${options.where}`;
    }

    let subrc = 0;
    let dbcnt = 0;
    try {
      if (this.trace === true) {
        console.log(sql);
      }

      const res = await this.modifying(sql);
      dbcnt = res?.rowCount || 0;
      if (dbcnt === 0) {
        subrc = 4;
      }
    } catch (error) {
      subrc = 4;
    }

    return {subrc, dbcnt};
  }

  public async update(options: DB.UpdateDatabaseOptions): Promise<{ subrc: number; dbcnt: number; }> {
    this.checkFatal();
    if (this.lazy === true && this.pool === undefined) {
      await this.connect();
    }

    const sql = `UPDATE ${options.table} SET ${options.set.join(", ")} WHERE ${options.where}`;

    let subrc = 0;
    let dbcnt = 0;
    try {
      if (this.trace === true) {
        console.log(sql);
      }

      const res = await this.modifying(sql);
      dbcnt = res?.rowCount || 0;
      if (dbcnt === 0) {
        subrc = 4;
      }
    } catch (error) {
      subrc = 4;
    }

    return {subrc, dbcnt};
  }

  public async insert(options: DB.InsertDatabaseOptions): Promise<{ subrc: number; dbcnt: number; }> {
    this.checkFatal();
    if (this.lazy === true && this.pool === undefined) {
      await this.connect();
    }

    const sql = `INSERT INTO ${options.table} (${options.columns.map(c => "\"" + c + "\"").join(",")}) VALUES (${options.values.join(",")})`;

    let subrc = 0;
    let dbcnt = 0;
    try {
      if (this.trace === true) {
        console.log(sql);
      }

      const res = await this.modifying(sql);
      dbcnt = res?.rowCount || 0;
    } catch (error) {
      if (this.trace === true) {
        console.dir(error);
      }
      // eg "UNIQUE constraint failed" errors
      subrc = 4;
    }
    return {subrc, dbcnt};
  }

  public async select(options: DB.SelectDatabaseOptions): Promise<DB.SelectDatabaseResult> {
    this.checkFatal();
    if (this.lazy === true && this.pool === undefined) {
      await this.connect();
    }

    let res: undefined | pg.QueryResult<any> = undefined;

    options.select = options.select.replace(/ UP TO (\d+) ROWS(.*)/i, "$2 LIMIT $1");
    if (options.primaryKey) {
      options.select = options.select.replace(/ ORDER BY PRIMARY KEY/i, " ORDER BY " + options.primaryKey.join(", "));
    } else {
      options.select = options.select.replace(/ ORDER BY PRIMARY KEY/i, "");
    }
    options.select = options.select.replace(/ ASCENDING/ig, " ASC");
    options.select = options.select.replace(/ DESCENDING/ig, " DESC");
    options.select = options.select.replace(/~/g, ".");
    options.select = options.select.replace(/ LIMIT 0/g, "");

    if (this.trace === true) {
      console.log(options.select);
    }

    try {
      res = await this.query(options.select);
    } catch (error) {
      // @ts-ignore
      if (abap.Classes["CX_SY_DYNAMIC_OSQL_SEMANTICS"] !== undefined) {
        // @ts-ignore
        throw await new abap.Classes["CX_SY_DYNAMIC_OSQL_SEMANTICS"]().constructor_({sqlmsg: error.message || ""});
      }
      throw error;
    }

    const rows = this.convert(res);

    return {rows: rows};
  }

  private convert(res: {rows: any[]}): DB.DatabaseRows {
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

  public async openCursor(options: DB.SelectDatabaseOptions): Promise<DB.DatabaseCursorCallbacks> {
    this.checkFatal();
    if (this.lazy === true && this.pool === undefined) {
      await this.connect();
    }

    const select = options.select.replace(/ UP TO (\d+) ROWS(.*)/i, "$2 LIMIT $1");

    // A pg-cursor blocks its connection until it is closed. Materialize cursors
    // opened inside a transaction so they both see the LUW's uncommitted changes
    // and allow further statements to execute while the cursor remains open.
    if (this.client !== undefined) {
      const result = await this.client.query(select);
      const rows = this.convert(result);
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

    const client = await this.pool!.connect();
    const cursor = client.query(new Cursor(select));
    return {
      fetchNextCursor: (packageSize: number) => this.fetchNextCursor.bind(this)(packageSize, cursor),
      closeCursor: () => this.closeCursor.bind(this)(cursor, client),
    };
  }

  private async fetchNextCursor(packageSize: number, cursor: Cursor): Promise<DB.SelectDatabaseResult> {
    const res = await cursor.read(packageSize);
    return {rows: this.convert({rows: res})};
  }

  private async closeCursor(cursor: any, client: any): Promise<void> {
    cursor.close(() => {
      client.release();
    });
  }

}
