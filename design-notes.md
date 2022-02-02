 # Design Notes

* `await` all method/form/fm calls
* as all method calls are `await` the default JS constructor cannot be used => `constructor_`
* Interfaces required for access to eg. constants
* CLAS locals_imp and locals_def are merged to one file, as abaplint points to the definitions which are skipped
* top level await possible via `.mjs` file extension
* No runtime creation of artifacts, requires rebuild
* Single threaded, as its running in node
* Database table buffering settings ignored, everything is always in the db

# Statements

* WRITE statement
  * no spaces between consequential WRITEs
  * WRITE for integer is not producing preceding spaces

