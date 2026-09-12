# @abaplint/database-duckdb

Transpiler database client for [DuckDB](https://duckdb.org), in-process and
columnar, via `@duckdb/node-api`. Nothing to install or start besides the
npm package, so it runs anywhere Node runs, CI included.

```js
import {DuckDBDatabaseClient} from "@abaplint/database-duckdb";

const db = new DuckDBDatabaseClient({path: ":memory:"}); // or a file, kept between runs
abap.context.databaseConnections["DEFAULT"] = db;
await db.connect();
await db.execute(schemas.pg); // DuckDB speaks the PostgreSQL DDL the transpiler emits
await db.execute(insert);
```

What the client does on top of the PostgreSQL dialect:

- `CREATE TABLE`: `NCHAR(n)` becomes `VARCHAR(n)`, DuckDB has no fixed-width character type.
- Trailing blanks: the runtime pads CHAR values and its SQL literals to the DDIC length.
  SQLite compares with `COLLATE RTRIM` and PostgreSQL `char(n)` ignores the padding,
  DuckDB `VARCHAR` keeps and compares it. Literals in INSERT/UPDATE/DELETE/WHERE are
  right-trimmed on the way in, so `WHERE key = 'X   '` finds `'X'`.
- The ABAP LUW: INSERT/UPDATE/DELETE open a transaction, COMMIT WORK and ROLLBACK WORK
  end it. A failing statement (duplicate key, `sy-subrc = 4`) aborts a DuckDB transaction
  and there are no savepoints to fence it, so the successful statements of the open LUW
  are kept and replayed into a fresh transaction after a failure.

See https://github.com/abaplint/transpiler for additional information
