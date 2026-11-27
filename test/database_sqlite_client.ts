import {expect} from "chai";
import {ABAP} from "../packages/runtime/src/";
import {SQLiteDatabaseClient} from "../packages/database-sqlite/src/";

describe("SQLite client, sql.js configuration", () => {
  before(() => {
    // @ts-ignore
    global.abap = new ABAP();
  });

  it("works without a sql.js configuration", async () => {
    const client = new SQLiteDatabaseClient({trace: false});
    await client.connect();
    const result = await client.select({select: "SELECT 1 AS one"});
    expect(result.rows).to.deep.equal([{one: 1}]);
    await client.disconnect();
  });

  it("forwards the sql.js configuration to initSqlJs", async () => {
    // sql.js caches the promise of its first initSqlJs() call for the lifetime
    // of the module, so a fresh client module (and a fresh sql.js) is loaded
    // to see the configuration reach initSqlJs()
    const clientModule = require.resolve("../packages/database-sqlite/src/");
    const sqlJsModule = require.resolve("sql.js", {paths: [clientModule]});
    delete require.cache[clientModule];
    delete require.cache[sqlJsModule];
    // eslint-disable-next-line @typescript-eslint/no-require-imports, @typescript-eslint/no-var-requires
    const fresh: typeof import("../packages/database-sqlite/src/") = require(clientModule);

    const located: string[] = [];
    const client = new fresh.SQLiteDatabaseClient({sqlJs: {
      locateFile: (file: string) => {
        located.push(file);
        return require.resolve("sql.js/dist/" + file, {paths: [clientModule]});
      },
    }});
    await client.connect();
    const result = await client.select({select: "SELECT 2 AS two"});
    expect(result.rows).to.deep.equal([{two: 2}]);
    await client.disconnect();
    expect(located).to.deep.equal(["sql-wasm.wasm"]);
  });
});
