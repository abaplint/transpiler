 # Design Notes

* `await` all method/form/fm calls
* as all method calls are `await` the default JS constructor cannot be used => `constructor_`
* Interfaces required for access to eg. constants
* CLAS locals_imp and locals_def are merged to one file, as abaplint points to the definitions which are skipped
* top level await possible via `.mjs` file extension
* No runtime creation of artifacts, requires rebuild
* Single threaded, as its running in node
* Database table buffering settings ignored, everything is always in the db
* No XSLT or Simple Transformations

# Source maps

Generated code carries source-map mappings back to the original ABAP. Conventions for
transpilers:

* The first `Chunk.append()` of a statement uses the statement's first token position
  (`node.getFirstToken().getStart()` or just `node`), so the generated statement maps
  to the start of the ABAP statement — what a debugger needs for breakpoints/stepping.
* Sub-expression chunks from `traversal.traverse(...)` are combined with
  `Chunk.appendChunk()`, never flattened via `chunk.getCode()` — flattening discards
  the expression-level mappings the traversal already produced.
* Trailing generated syntax (`;`, `)`, `.clear()`, ...) is appended with the statement's
  last token (`node.getLastToken()`), not `getLastToken().getEnd()` which points one
  column past the statement.
* `statements/clear.ts` is the reference implementation of these conventions.

# Statements

* WRITE statement
  * no spaces between consequential WRITEs
  * WRITE for integer is not producing preceding spaces

