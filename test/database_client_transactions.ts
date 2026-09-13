import {expect} from "chai";
import {ABAP, MemoryConsole} from "../packages/runtime/src";
import {PostgresDatabaseClient} from "../packages/database-pg/src";
import {SnowflakeDatabaseClient} from "../packages/database-snowflake/src";
import {SQLiteDatabaseClient} from "../packages/database-sqlite/src";
import {DuckDBDatabaseClient} from "../packages/database-duckdb/src";

describe("Database client transactions", () => {

  beforeEach(() => {
    // SQLite uses the global runtime to set sy-dbsys during connect.
    (global as any).abap = new ABAP({console: new MemoryConsole()});
  });

  it("SQLite can roll back after a failed commit", async () => {
    const db = new SQLiteDatabaseClient();
    await db.connect();
    await db.execute([
      "PRAGMA foreign_keys = ON",
      "CREATE TABLE parent(id INTEGER PRIMARY KEY)",
      "CREATE TABLE child(parent_id INTEGER, "
        + "FOREIGN KEY(parent_id) REFERENCES parent(id) DEFERRABLE INITIALLY DEFERRED)",
    ]);
    await db.beginTransaction();
    await db.execute("INSERT INTO child VALUES (1)");

    let failed = false;
    try {
      await db.commit();
    } catch {
      failed = true;
    }
    expect(failed).to.equal(true);

    await db.rollback();
    const result = await db.select({select: "SELECT * FROM child"});
    expect(result.rows).to.have.length(0);

    // The internal transaction state must also allow a new LUW to start.
    await db.beginTransaction();
    await db.rollback();
    await db.disconnect();
  });

  it("DuckDB, the LUW continues after a failed statement", async () => {
    // DuckDB has no savepoints, a failed statement aborts the whole transaction,
    // the client replays the successful statements of the LUW into a new one
    const db = new DuckDBDatabaseClient();
    await db.connect();
    await db.execute("CREATE TABLE t(id VARCHAR(2) PRIMARY KEY, txt VARCHAR(10))");

    let res = await db.insert({table: "t", columns: ["id", "txt"], values: ["'A '", "'first     '"]});
    expect(res).to.deep.equal({subrc: 0, dbcnt: 1});
    res = await db.insert({table: "t", columns: ["id", "txt"], values: ["'A '", "'dup       '"]});
    expect(res.subrc).to.equal(4);
    res = await db.insert({table: "t", columns: ["id", "txt"], values: ["'B '", "'second    '"]});
    expect(res).to.deep.equal({subrc: 0, dbcnt: 1});
    res = await db.update({table: "t", set: ["txt = 'changed   '"], where: "id = 'A  '"});
    expect(res).to.deep.equal({subrc: 0, dbcnt: 1});

    // trailing blanks in literals are trimmed, the padded key finds the row
    let result = await db.select({select: "SELECT * FROM t WHERE id = 'A   ' ORDER BY PRIMARY KEY", primaryKey: ["id"]});
    expect(result.rows).to.deep.equal([{id: "A", txt: "changed"}]);

    await db.commit();
    result = await db.select({select: "SELECT id FROM t ORDER BY id"});
    expect(result.rows).to.deep.equal([{id: "A"}, {id: "B"}]);

    // ROLLBACK WORK discards the open LUW
    await db.delete({table: "t", where: "id = 'B'"});
    await db.rollback();
    result = await db.select({select: "SELECT COUNT(*) AS cnt FROM t"});
    expect(result.rows).to.deep.equal([{cnt: 2}]);

    await db.disconnect();
  });

  it("Snowflake can roll back after a failed commit", async () => {
    const calls: string[] = [];
    const db = new SnowflakeDatabaseClient({account: "test", username: "test"});
    (db as any).connection = {
      execute: (options: any) => {
        calls.push(options.sqlText);
        if (options.sqlText === "COMMIT") {
          options.complete(new Error("commit failed"), undefined, undefined);
        } else {
          options.complete(undefined, undefined, []);
        }
      },
    };

    await db.beginTransaction();
    let failed = false;
    try {
      await db.commit();
    } catch {
      failed = true;
    }
    expect(failed).to.equal(true);

    await db.rollback();
    expect(calls).to.deep.equal(["BEGIN", "COMMIT", "ROLLBACK"]);
  });

  it("PostgreSQL releases a client when BEGIN fails", async () => {
    let connectCalls = 0;
    let releaseCalls = 0;
    let failBegin = true;
    const client = {
      query: async (sql: string) => {
        if (sql === "BEGIN" && failBegin) {
          throw new Error("begin failed");
        }
        return {rows: [], rowCount: 0};
      },
      release: () => { releaseCalls++; },
    };
    const pool = {
      connect: async () => {
        connectCalls++;
        return client;
      },
    };
    const db = new PostgresDatabaseClient({
      user: "test", host: "test", database: "test", password: "test", port: 5432,
    });
    await db.connect(pool as any);

    let failed = false;
    try {
      await db.beginTransaction();
    } catch {
      failed = true;
    }
    expect(failed).to.equal(true);
    expect(releaseCalls).to.equal(1);

    failBegin = false;
    await db.beginTransaction();
    expect(connectCalls).to.equal(2);
    await db.rollback();
  });

  it("PostgreSQL, a failed commit is fatal", async () => {
    const calls: string[] = [];
    let releaseCalls = 0;
    const client = {
      query: async (sql: string) => {
        calls.push(sql);
        if (sql === "COMMIT") {
          throw new Error("commit failed");
        }
        return {rows: [], rowCount: 0};
      },
      release: () => { releaseCalls++; },
    };
    const pool = {connect: async () => client};
    const db = new PostgresDatabaseClient({
      user: "test", host: "test", database: "test", password: "test", port: 5432,
    });
    await db.connect(pool as any);
    await db.beginTransaction();

    let message = "";
    try {
      await db.commit();
    } catch (error: any) {
      message = error.message;
    }
    expect(message).to.contain("COMMIT failed");
    expect(message).to.contain("commit failed");
    // postgres already ended the transaction, hand the connection straight back
    expect(calls).to.deep.equal(["BEGIN", "COMMIT"]);
    expect(releaseCalls).to.equal(1);

    // every further use must crash instead of degrading into sy-subrc = 4
    const operations = [
      () => db.insert({table: "example", columns: ["id"], values: ["1"]}),
      () => db.update({table: "example", where: "id = 2", set: ["id = 1"]}),
      () => db.delete({table: "example", where: "id = 1"}),
      () => db.select({select: "SELECT * FROM example"}),
      () => db.openCursor({select: "SELECT * FROM example"}),
      () => db.execute("SELECT 1"),
      () => db.beginTransaction(),
    ];
    for (const operation of operations) {
      let thrown = "";
      try {
        await operation();
      } catch (error: any) {
        thrown = error.message;
      }
      expect(thrown).to.contain("COMMIT failed");
    }
    // and none of them reached the database
    expect(calls).to.deep.equal(["BEGIN", "COMMIT"]);
  });

  it("PostgreSQL transaction cursors see uncommitted changes", async () => {
    let connectCalls = 0;
    let releaseCalls = 0;
    let transactionRows: any[] = [];
    const transactionClient = {
      query: async (sql: string) => {
        if (sql.startsWith("INSERT")) {
          transactionRows = [{id: 1}];
          return {rows: [], rowCount: 1};
        } else if (sql.startsWith("SELECT")) {
          return {rows: transactionRows, rowCount: transactionRows.length};
        }
        return {rows: [], rowCount: 0};
      },
      release: () => { releaseCalls++; },
    };
    const separateClient = {
      query: () => ({
        read: async () => [],
        close: (callback: () => void) => callback(),
      }),
      release: () => undefined,
    };
    const pool = {
      connect: async () => {
        connectCalls++;
        return connectCalls === 1 ? transactionClient : separateClient;
      },
    };
    const db = new PostgresDatabaseClient({
      user: "test", host: "test", database: "test", password: "test", port: 5432,
    });
    await db.connect(pool as any);

    await db.insert({table: "example", columns: ["id"], values: ["1"]});
    const cursor = await db.openCursor({select: "SELECT * FROM example"});
    const first = await cursor.fetchNextCursor(1);
    const second = await cursor.fetchNextCursor(1);

    expect(first.rows).to.deep.equal([{id: 1}]);
    expect(second.rows).to.deep.equal([]);
    expect(connectCalls).to.equal(1);
    await cursor.closeCursor();
    await db.rollback();
    expect(releaseCalls).to.equal(1);
  });

});
