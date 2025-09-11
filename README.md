# transpiler

[![npm (scoped)](https://img.shields.io/npm/v/@abaplint/runtime?label=%40abaplint%2Fruntime)](https://www.npmjs.com/package/@abaplint/runtime)
[![npm (scoped)](https://img.shields.io/npm/v/@abaplint/transpiler?label=%40abaplint%2Ftranspiler)](https://www.npmjs.com/package/@abaplint/transpiler)
[![npm (scoped)](https://img.shields.io/npm/v/@abaplint/transpiler-cli?label=%40abaplint%2Ftranspiler-cli)](https://www.npmjs.com/package/@abaplint/transpiler-cli)
[![CI](https://github.com/abaplint/transpiler/workflows/CI/badge.svg)](https://github.com/abaplint/transpiler/actions)

Input must be ABAP 7.02 syntax, use abaplint [downport](https://rules.abaplint.org/downport/) rule to automatically change higher syntax before transpiling. Support has been added for many 750 statements and expressions, check https://syntax.abaplint.org for compatibility.

Target is [ES6](http://es6-features.org)

Playground: https://transpiler.abaplint.org

Connecting to a database is optional, currently the following databases have drivers:
* [SQLite](https://www.sqlite.org) via [@abaplint/database-sqlite](https://www.npmjs.com/package/@abaplint/database-sqlite)
* [PostgreSQL](https://www.postgresql.org) via [@abaplint/database-pg](https://www.npmjs.com/package/@abaplint/database-pg)
* [Snowflake](https://www.snowflake.com/) via [@abaplint/database-snowflake](https://www.npmjs.com/package/@abaplint/database-snowflake)

## Reuse library

https://open-abap.org

## Examples
* [abapGit](https://github.com/abapGit/abapGit) runs unit tests on every push
* [abap2UI5](https://github.com/abap2UI5/abap2UI5) runs unit tests on every push
* [abap-file-formats-tools](https://github.com/SAP/abap-file-formats-tools) runs unit tests on every push
* [Exercism ABAP Track](https://exercism.org/tracks/abap)

## Technical
* `SY-SYSID` = `ABC`
* `SY-MANDT` = `123`
* Fixed point arithmetic is always enabled
* Encoding = [UCS-2](https://en.wikipedia.org/wiki/Universal_Coded_Character_Set)
* Time zone = [UTC](https://en.wikipedia.org/wiki/Coordinated_Universal_Time)

## Development

Prerequisites: [Node.js](https://nodejs.org) 16+

For testing locally run `npm run install && npm test`