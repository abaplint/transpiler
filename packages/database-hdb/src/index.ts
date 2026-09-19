import {DB} from "@abaplint/runtime";
import * as hdb from "hdb";

export type ConnectionSettings = {
  host: string,
  port: number,
  user: string,
  password: string,
  /** the schema the tables live in, created if absent */
  schema?: string,
  /** hdb defaults to 128 KB and refuses a bigger statement; the driver maximum is 2^30-1 */
  packetSize?: number,
};

/** ABAP pads its CHAR literals to the DDIC length; HANA compares the padding.
 *  Trim inside single quotes only, and leave an escaped quote ('') alone. */
function trimLiterals(sql: string): string {
  return sql.replace(/'((?:[^']|'')*)'/g, (_m, inner: string) => "'" + inner.replace(/ +$/, "") + "'");
}

/** Every identifier this client sends goes to HANA quoted in UPPER case.
 *
 *  This is about the table DDL and the statements, not about the schema name:
 *  connect() creates and sets the schema exactly as given, so a caller that
 *  asks for a lower-case schema gets one.
 *
 *  The SQL reaching the client comes in two shapes: most references are
 *  written unquoted and lower case, which HANA folds to upper by itself and
 *  which therefore already match, and some arrive quoted and lower case, like
 *  `INSERT INTO "cross" ("type", ...)`, which do not, and fail with
 *  `Could not find table/view cross`. Folding the contents of every
 *  double-quoted identifier to upper case makes both shapes land on one name.
 *
 *  The schema has to be created quoted-upper for the same reason plus one
 *  more: a column called `cross` is a reserved word in HANA and cannot be
 *  created unquoted at all. SYS.RESERVED_KEYWORDS also holds END, GROUP,
 *  ORDER and START, all plausible ABAP field names.
 *
 *  **A double quote is not always an identifier**, which is why this walks the
 *  statement instead of running a regular expression over it: a column holding
 *  the JSON value {"draft":"kept"} is inside single quotes, and a regex folds
 *  it into {"DRAFT":"KEPT"} -- corrupting data rather than SQL, invisibly to
 *  any test that checks the SQL. The `true`/`false` rewrite is inside the same
 *  walk for the same reason: `'say true please'` is a value.
 */
function foldIdentifiers(sql: string): string {
  let out = "";
  let plainRun = "";
  const flush = () => {
    out += plainRun.replace(/\btrue\b/gi, "1 = 1").replace(/\bfalse\b/gi, "1 = 0");
    plainRun = "";
  };
  let i = 0;
  while (i < sql.length) {
    const c = sql[i];
    if (c === "'") {                       // a value: copy it exactly
      let j = i + 1;
      while (j < sql.length) {
        if (sql[j] === "'" && sql[j + 1] === "'") { j += 2; continue; }
        if (sql[j] === "'") { j += 1; break; }
        j += 1;
      }
      flush();
      out += sql.slice(i, j);
      i = j;
    } else if (c === "\"") {               // an identifier: fold it
      const close = sql.indexOf("\"", i + 1);
      if (close === -1) { plainRun += sql.slice(i); break; }
      const id = sql.slice(i + 1, close);
      flush();
      out += /^[A-Za-z_][A-Za-z_0-9]*$/.test(id) ? "\"" + id.toUpperCase() + "\"" : sql.slice(i, close + 1);
      i = close + 1;
    } else {
      plainRun += c;
      i += 1;
    }
  }
  flush();
  return out;
}

/** a row of the seam is flat */
function plain(value: any): any {
  if (typeof value === "bigint") {
    return Number(value);
  }
  if (value === null || value === undefined) {
    return value;
  }
  if (Buffer.isBuffer(value)) {
    return value.toString("utf8");
  }
  if (value instanceof Date) {
    return value;
  }
  if (typeof value === "object" && typeof value.toString === "function") {
    return value.toString();
  }
  return value;
}

export class HanaDatabaseClient implements DB.DatabaseClient {
  /** what sy-dbsys reports; a system running on HANA says HDB */
  public readonly name = "HDB";
  private readonly config: ConnectionSettings;
  private readonly schema: string;
  private readonly trace: boolean | undefined;
  private client: any | undefined;
  private inTransaction = false;

  public constructor(input: ConnectionSettings & {trace?: boolean}) {
    this.config = input;
    this.schema = input.schema ?? "ABAP";
    this.trace = input.trace;
  }

  public async connect(): Promise<void> {
    const size = this.config.packetSize ?? 1024 * 1024 * 64;
    this.client = hdb.createClient({
      host: this.config.host,
      port: this.config.port,
      user: this.config.user,
      password: this.config.password,
      packetSize: size,
      packetSizeLimit: size,
    });
    await new Promise<void>((resolve, reject) =>
      this.client.connect((err: Error) => (err ? reject(err) : resolve())));
    // COMMIT and ROLLBACK are meaningless while the session autocommits
    this.client.setAutoCommit(false);
    await this.run(`CREATE SCHEMA "${this.schema}"`).catch(() => undefined);
    await this.run(`SET SCHEMA "${this.schema}"`);
  }

  public async disconnect(): Promise<void> {
    await this.commit();
    this.client?.end();
    this.client = undefined;
  }

  public async execute(sql: string | string[]): Promise<void> {
    if (Array.isArray(sql)) {
      for (const s of sql) {
        await this.execute(s);
      }
      return;
    }
    if (sql === "") {
      return;
    }
    // Open the LUW here, because this client has autocommit off: without
    // this, `inTransaction` stays false for anything sent through execute(),
    // commit() returns at its first line, and the work is lost at
    // disconnect -- which the interface documents as an implicit commit.
    // A single connection never sees it, since a session reads its own
    // uncommitted rows. The other drivers run in autocommit, so they cannot
    // lose it this way. beginTransaction() is a no-op when one is open.
    await this.beginTransaction();
    const folded = foldIdentifiers(sql);
    if (this.trace) {
      console.log(folded);
    }
    await this.run(folded);
  }

  public async beginTransaction(): Promise<void> {
    // hdb opens one implicitly on the first statement once autocommit is off;
    // the flag is what commit() and rollback() read
    this.inTransaction = true;
  }

  public async commit(): Promise<void> {
    if (this.inTransaction === false) {
      return;
    }
    await new Promise<void>((resolve, reject) =>
      this.client.commit((err: Error) => (err ? reject(err) : resolve())));
    this.inTransaction = false;
  }

  public async rollback(): Promise<void> {
    if (this.inTransaction === false) {
      return;
    }
    await new Promise<void>((resolve, reject) =>
      this.client.rollback((err: Error) => (err ? reject(err) : resolve())));
    this.inTransaction = false;
  }

  public async insert(options: {table: string, columns: string[], values: string[]}): Promise<{subrc: number, dbcnt: number}> {
    const sql = trimLiterals(
      `INSERT INTO ${options.table} (${options.columns.join(",")}) VALUES (${options.values.join(",")})`);
    try {
      const dbcnt = await this.modifying(sql);
      return {subrc: 0, dbcnt};
    } catch (error) {
      if (this.trace) {
        console.error(error);
      }
      return {subrc: 4, dbcnt: 0};
    }
  }

  public async update(options: {table: string, set: string[], where: string}): Promise<{subrc: number, dbcnt: number}> {
    const sql = trimLiterals(`UPDATE ${options.table} SET ${options.set.join(", ")} WHERE ${options.where}`);
    try {
      const dbcnt = await this.modifying(sql);
      return {subrc: dbcnt === 0 ? 4 : 0, dbcnt};
    } catch (error) {
      if (this.trace) {
        console.error(error);
      }
      return {subrc: 4, dbcnt: 0};
    }
  }

  public async delete(options: {table: string, where: string}): Promise<{subrc: number, dbcnt: number}> {
    let sql = `DELETE FROM ${options.table}`;
    if (options.where !== "") {
      sql += ` WHERE ${options.where}`;
    }
    try {
      const dbcnt = await this.modifying(trimLiterals(sql));
      return {subrc: dbcnt === 0 ? 4 : 0, dbcnt};
    } catch (error) {
      if (this.trace) {
        console.error(error);
      }
      return {subrc: 4, dbcnt: 0};
    }
  }

  public async select(options: {select: string, primaryKey?: string[]}): Promise<{rows: any[]}> {
    return {rows: await this.query(this.rewrite(options.select, options.primaryKey))};
  }

  /** the seam allows a cursor served by reading and slicing, as in database-duckdb */
  public async openCursor(options: {select: string, primaryKey?: string[]}): Promise<any> {
    const rows = await this.query(this.rewrite(options.select, options.primaryKey));
    let offset = 0;
    return {
      fetchNextCursor: async (packageSize: number) => {
        const slice = rows.slice(offset, offset + packageSize);
        offset += packageSize;
        return {rows: slice};
      },
      closeCursor: async () => undefined,
    };
  }

  /** ABAP SQL as the runtime emits it, into what HANA takes */
  private rewrite(select: string, primaryKey?: string[]): string {
    let s = select.replace(/ UP TO (\d+) ROWS(.*)/i, "$2 LIMIT $1");
    s = primaryKey
      ? s.replace(/ ORDER BY PRIMARY KEY/i, " ORDER BY " + primaryKey.join(", "))
      : s.replace(/ ORDER BY PRIMARY KEY/i, "");
    s = s.replace(/ ASCENDING/ig, " ASC").replace(/ DESCENDING/ig, " DESC")
      .replace(/~/g, ".").replace(/ LIMIT 0/g, "");
    return trimLiterals(s);
  }

  private async query(sql: string): Promise<any[]> {
    const folded = foldIdentifiers(sql);
    if (this.trace) {
      console.log(folded);
    }
    const rows = await this.run(folded);
    // HANA holds the names in upper case and the runtime looks a column up by
    // the lower-case name it asked for, so fold down on the way back: one
    // rule, applied twice
    return (rows ?? []).map((r: any) => {
      const row: any = {};
      for (const k of Object.keys(r)) {
        row[k.toLowerCase()] = plain(r[k]);
      }
      return row;
    });
  }

  /** one modifying statement; the count of rows it touched */
  private async modifying(sql: string): Promise<number> {
    await this.beginTransaction();
    const folded = foldIdentifiers(sql);
    if (this.trace) {
      console.log(folded);
    }
    const affected = await this.run(folded);
    return typeof affected === "number" ? affected : Number(affected ?? 0);
  }

  private run(sql: string): Promise<any> {
    return new Promise((resolve, reject) =>
      this.client.exec(sql, (err: Error, result: any) => (err ? reject(err) : resolve(result))));
  }
}
