# @abaplint/database-sqlite

@abaplint/database-sqlite

## Usage

```ts
import {SQLiteDatabaseClient} from "@abaplint/database-sqlite";

const client = new SQLiteDatabaseClient({
  trace: false,
  // optional, passed to initSqlJs() as is
  sqlJs: {locateFile: file => "/assets/" + file},
});
await client.connect();
```

`sqlJs` takes the sql.js configuration (`locateFile`, `wasmBinary`, ...).
Without it sql.js resolves `sql-wasm.wasm` relative to the page, which fails
in a browser as soon as the page is not served from the site root.

See https://github.com/abaplint/transpiler for additional information
