# Design Notes


* `await` all method/form/fm calls
* as all method calls are `await` the default JS constructor cannot be used => `constructor_`
* Interfaces required foraccess to eg. constants
* CLAS locals_imp and locals_def are merged to one file, as abaplint points to the definitions which are skipped
* top level await possible via `.mjs` file extension

# Difference to SAP compiler (a feature not a bug)

* write statement
- no spaces between consequential calling of write
- write on integer is not producing preceding spaces
 