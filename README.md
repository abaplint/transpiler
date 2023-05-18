# transpiler

[![npm (scoped)](https://img.shields.io/npm/v/@abaplint/runtime?label=%40abaplint%2Fruntime)](https://www.npmjs.com/package/@abaplint/runtime)
[![npm (scoped)](https://img.shields.io/npm/v/@abaplint/transpiler?label=%40abaplint%2Ftranspiler)](https://www.npmjs.com/package/@abaplint/transpiler)
[![npm (scoped)](https://img.shields.io/npm/v/@abaplint/transpiler-cli?label=%40abaplint%2Ftranspiler-cli)](https://www.npmjs.com/package/@abaplint/transpiler-cli)
[![CI](https://github.com/abaplint/transpiler/workflows/CI/badge.svg)](https://github.com/abaplint/transpiler/actions)

Input must be ABAP 7.02 syntax, use [downport](https://rules.abaplint.org/downport/) to automatically change higher syntax before transpiling.

Target is [ES6](http://es6-features.org)

Database support: [SQLite](https://www.sqlite.org) & [Postgres](https://www.postgresql.org)(wip)

Playground: https://transpiler.abaplint.org

## Technical
* Fixed point arithmetic is always enabled
* Everything will run as unicode, targeting [UCS-2](https://en.wikipedia.org/wiki/Universal_Coded_Character_Set)
* Endianness ?
* Time zone = [UTC](https://en.wikipedia.org/wiki/Coordinated_Universal_Time)
* `SY-SYSID` = `ABC`
* `SY-MANDT` = `123`

## Testing Locally

Prerequisites: [Node.js](https://nodejs.org) 16+

`npm run install && npm test`
