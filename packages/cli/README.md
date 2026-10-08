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
are rejected; a project object replaces a library object of the same type/name.

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
