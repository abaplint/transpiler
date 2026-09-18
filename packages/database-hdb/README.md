# @abaplint/database-hdb

SAP HANA driver for the [abaplint transpiler](https://github.com/abaplint/transpiler),
over the [`hdb`](https://www.npmjs.com/package/hdb) client.

HANA is the database SAP's own AMDP targets, and the one a transpiled program
can be compared against: it is the only backend where a difference between what
the transpiler does and what a system running the same ABAP does is visible at
all.

```js
const {HanaDatabaseClient} = require("@abaplint/database-hdb");

const client = new HanaDatabaseClient({
  host: "localhost", port: 39017, user: "SYSTEM", password: "...", schema: "ABAP",
});
await client.connect();
```

`sy-dbsys` reports `HDB`.

## What this client does that the SQLite one does not

- **Identifiers are folded to upper case**, by walking each statement rather
  than by a regular expression over it: a double quote is not always an
  identifier, and a regex rewrites the insides of string literals too.
- **`true` / `false` become `1 = 1` / `1 = 0`** -- HANA has no boolean literal
  in that position. Same walk, same reason.
- **Literals are right-trimmed**, quote-aware: ABAP pads `CHAR` to its DDIC
  length, SQLite ignores the padding through `COLLATE RTRIM` and HANA does
  not, so `WHERE K = 'A         '` would find nothing.
- **The packet size is raised.** `hdb` defaults to 128 KB and refuses a larger
  statement; a seed row carrying an ABAP source or an SMW0 object as hex is
  megabytes.
- **Autocommit is switched off**, or `COMMIT` and `ROLLBACK` mean nothing.

The DDL needs no dialect of its own: HANA takes the PostgreSQL schema the
transpiler already generates.

## Running against SAP HANA Express

HANA Express is free for development and runs in Docker. It is a 4.5 GB image
that wants a few GB of memory and about three minutes to come up, so it does
not belong in a CI stack next to PostgreSQL; the tests here are gated on the
connection environment being set, the way the Snowflake ones are.
