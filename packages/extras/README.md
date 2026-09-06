# @abaplint/transpiler-extras

Additional object type support for the [abaplint transpiler](https://github.com/abaplint/transpiler), delivered as transpiler plugins.

## License

This package is source-available under the [PolyForm Noncommercial License 1.0.0](./LICENSE.md):
free for any noncommercial purpose. Commercial use requires a separate commercial license, contact hello@heliconialabs.com

Required Notice: Copyright Heliconia Labs ApS (hello@heliconialabs.com)

Note that this license differs from the rest of the monorepo, which is MIT.

## Usage

Install the package next to the transpiler CLI,

```
npm install @abaplint/transpiler-extras
```

no configuration is needed, the CLI automatically detects and uses the package when it is installed.

CDS SELECT views over `TABL` sources generate database views for SQLite, PostgreSQL,
and Snowflake. Source aliases, renamed field projections, inner/left/right/cross
joins, `ON` conditions, `WHERE`, and `DISTINCT` are preserved. Conditions support
field references, string/integer literals, comparisons, boolean operators, `LIKE`,
`BETWEEN`, and `IS NULL`.

Unresolved sources and unsupported SQL view constructs (such as parameters,
association paths, computed projections, grouping, or unions) fail with a CDS
view diagnostic instead of generating a partial query. Abstract/custom entities
do not create database views.
