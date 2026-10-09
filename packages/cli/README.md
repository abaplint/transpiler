# @abaplint/transpiler-cli

Transpiler CLI

## Examples

Call `abap_transpile` on the command line, see example in

https://github.com/larshp/abap-advent-2020/blob/main/package.json#L8

## Output layout

The CLI writes generated project files into `output_folder/project/`. All
`input_folder` entries belong to the same project, including an array of source
folders. Original ABAP sources stay in their input directories.

Each ABAP library gets its own folder under `output_folder`. The folder name is
the repository basename from `url` (without a trailing slash or `.git`), or the
basename of `folder` for a local-only library. Set `libs[].name` to override it:

```json
{
  "input_folder": ["src", "extra"],
  "output_folder": "output",
  "libs": [
    {"url": "https://github.com/open-abap/open-abap-core"},
    {"folder": "./deps/utilities", "name": "utilities"}
  ],
  "write_unit_tests": true,
  "write_source_map": true,
  "options": {"addCommonJS": true}
}
```

```text
output/
  init.mjs
  _init.mjs
  _top.mjs
  index.mjs
  _unit_open.mjs
  project/
    zapp.prog.mjs
    zapp.prog.mjs.map
  open-abap-core/
    ...
  utilities/
    ...
```

Names must be valid single directory names on Windows and POSIX. Names are
unique without regard to case, and `project` is reserved. A conflicting derived
name needs an explicit `libs[].name`. Duplicate ABAP objects across libraries
are rejected by default; a project object replaces a library object of the same type/name.

Class locals, test classes, function groups, MIME data, and
source maps follow their owning object. Source maps include ABAP source content,
including libraries loaded from temporary Git checkouts. Imports and registered
MIME filenames reference the new paths.

Shared initialization scripts and test runners remain at the output root.
Run programs with `node output/project/zapp.prog.mjs`; run project unit tests
with `node output/index.mjs`. When migrating from flat output, rebuild a clean
generated output directory and update scripts that reference individual
modules or assets. The CLI does not remove old generated files or unrelated
files automatically.

The transpiler library retains flat output unless its caller supplies an output
folder map as the third argument to `Transpiler.run()`. This layout is enabled
automatically by the CLI.

## Duplicate dependencies

By default, two libraries defining the same ABAP object (type and name) cause
an `Ambiguous dependency object` error. To keep existing dependency repositories
and skip later copies, set this top-level option in `abap_transpile.json`:

```json
{
  "skip_duplicate_dependencies": true
}
```

The first library in `libs` containing an object wins. Every file of that
object in later libraries is skipped, including metadata, class locals, and
binary assets. The CLI warns once per skipped object per library, naming the
selected library. Imports, output folders, and source maps use the selected
copy. Project objects still take precedence over dependencies, and shared
`package.devc.xml` metadata keeps its existing behavior.

Library order now affects generated output. Different definitions can change
DDIC types, available methods, and runtime behavior, or cause syntax errors in
code expecting the skipped version. This option does not check that copies are
equivalent or suppress syntax checking. Put the intended provider first and
run your application's tests. For selective control, use `libs[].exclude_filter`
to exclude only the unwanted object's files instead. Use a clean generated
output directory when changing providers, since old output is not removed.
