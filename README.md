# transpiler

[![npm (scoped)](https://img.shields.io/npm/v/@abaplint/runtime?label=%40abaplint%2Fruntime)](https://www.npmjs.com/package/@abaplint/runtime)
[![npm (scoped)](https://img.shields.io/npm/v/@abaplint/transpiler?label=%40abaplint%2Ftranspiler)](https://www.npmjs.com/package/@abaplint/transpiler)
[![npm (scoped)](https://img.shields.io/npm/v/@abaplint/transpiler-cli?label=%40abaplint%2Ftranspiler-cli)](https://www.npmjs.com/package/@abaplint/transpiler-cli)
[![npm (scoped)](https://img.shields.io/npm/v/@abaplint/abap-loader?label=%40abaplint%2Fabap-loader)](https://www.npmjs.com/package/@abaplint/abap-loader)
[![CI](https://github.com/abaplint/transpiler/workflows/CI/badge.svg)](https://github.com/abaplint/transpiler/actions)


Target is [ES6](http://es6-features.org)

Planned database support: [SQLite](https://www.sqlite.org) & [HANA Express](https://www.sap.com/cmp/td/sap-hana-express-edition.html)

Playground: https://transpiler.abaplint.org

## Technical
* Fixed point arithmetic is always enabled
* Everything will run as unicode, targeting [UCS-2](https://en.wikipedia.org/wiki/Universal_Coded_Character_Set)
* Endianess ?
* Time zone = [UTC](https://en.wikipedia.org/wiki/Coordinated_Universal_Time)
* sysid = "ABC"
* client = "123"

## Running

Prerequsites: [Node.js](https://nodejs.org/) 14.8.0+

`npm install`

`npm test`
