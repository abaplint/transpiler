# Design Notes


* `await` all method/form/fm calls
* as all method calls are `await` the default JS constructor cannot be used => `constructor_`
* Interfaces required foraccess to eg. constants
* CLAS locals_imp and locals_def are merged to one file, as abaplint points to the definitions which are skipped
* top level await possible via `.mjs` file extension