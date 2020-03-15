# transpiler

[![npm (scoped)](https://img.shields.io/npm/v/@abaplint/runtime?label=%40abaplint%2Fruntime)](https://www.npmjs.com/package/@abaplint/runtime)
[![npm (scoped)](https://img.shields.io/npm/v/@abaplint/transpiler?label=%40abaplint%2Ftranspiler)](https://www.npmjs.com/package/@abaplint/transpiler)
[![CI](https://github.com/abaplint/transpiler/workflows/CI/badge.svg)](https://github.com/abaplint/transpiler/actions)

Proof of concept, things might change

Type information will be required, to be added later, also more work in [abaplint](https://github.com/abaplint/abaplint) needed for this

Target is [ES6](http://es6-features.org)

Planned database support: [SQLite](https://www.sqlite.org) & [HANA Express](https://www.sap.com/cmp/td/sap-hana-express-edition.html)

## Technical
* Fixed point arithmetic is always enabled
* Everything will run as unicode, targeting [UCS-2](https://en.wikipedia.org/wiki/Universal_Coded_Character_Set)
* Endianess ?
* Time zone = [UTC](https://en.wikipedia.org/wiki/Coordinated_Universal_Time)
* sysid = "ABC"
* client = "123"

## Running

Prerequsites: [Node.js](https://nodejs.org/) 10+

`npm install`

`npm test`