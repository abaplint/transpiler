# Plan: separate output folders for the project and each dependency

## Goal and expected layout

Save files generated from the configured `input_folder` in `<output_folder>/project/`, and files generated from each ABAP library (`libs`) in `<output_folder>/<dependency-name>/`. Keep shared bootstrap and test entry points in the output root. This applies to ABAP sources and dependencies, not installed npm packages.

For example, a dependency on `https://github.com/open-abap/open-abap-core` produces:

```text
output/
  init.mjs
  _init.mjs
  _top.mjs
  index.mjs                 # when write_unit_tests is enabled
  _unit_open.mjs            # when write_unit_tests is enabled
  project/
    zapp.prog.mjs
    zapp.prog.mjs.map        # when write_source_map is enabled
    zcl_app.clas.mjs
    zcl_app.clas.testclasses.mjs
  open-abap-core/
    kernel_unit_runner.clas.mjs
    kernel_unit_runner.clas.mjs.map
    ...
  another-library/
    ...
```

All outputs belonging to an object must follow its owning project or dependency folder, including class locals, test classes, function groups, plugin outputs, MIME data, and source maps. Groups with no emitted files need no empty directory.

### Recommended project folder policy

Use the fixed name `project` for outputs from `input_folder`. It describes ownership clearly and stays stable if the repository, checkout directory, or source directory is renamed. Treat a string or array of input folders as one project and combine their generated outputs under `project/`; retain the existing generated filenames rather than recreating the source directory tree. Original ABAP files remain in their input directories. Reserve `project` as a dependency folder name, case-insensitively; a library with that derived name must set another `libs[].name`. No additional project-naming configuration is needed for this change.

## Original implementation

- `packages/cli/src/index.ts`: `loadLib()` flattens library files; `build()` registers them as dependencies without retaining library identity; `writeObjects()` writes everything to the root and prepends a root-relative program bootstrap.
- `packages/cli/src/types.ts`: library configuration has URL/folder/filter fields, but no dependency name.
- `packages/cli/src/file_operations.ts`: the writer assumes destination directories already exist.
- `packages/transpiler/src/handlers/handle_abap.ts` and `packages/transpiler/src/initialization.ts` generate imports assuming flat output. `packages/transpiler/src/unit_test.ts` also generates file paths.
- `packages/transpiler/src/types.ts`: each output has an object identifier suitable for finding its owner, including outputs whose filename differs from the source filename.

## Implementation steps

### 1. Define project and dependency naming

- [x] Use the fixed `project/` directory for all project outputs, including builds without dependencies and configurations with multiple input folders.
- [x] Add optional `libs[].name` in `packages/cli/src/types.ts` for an explicit output folder name.
- [x] Resolve names in this order: explicit name, repository basename from URL (without trailing slash or `.git`), then local folder basename for folder-only libraries. Support HTTPS and Git SSH URLs.
- [x] Resolve names before cloning; never use the randomly named temporary checkout.
- [x] Validate names as single directory components on Windows and POSIX. Reject empty names, separators, traversal, and invalid/reserved filenames with an actionable error.
- [x] Reject the reserved name `project` and duplicate dependency names, including case-insensitive collisions; suggest an explicit name instead of silently merging libraries.
- [x] Regenerate `packages/cli/schema.json` with the CLI's `npm run schema` script.

### 2. Preserve project and dependency ownership through loading

- [x] Refactor `loadLib()` to return named library groups with source files and source-location metadata.
- [x] Preserve existing folder resolution, cloning, filters, dependency test exclusion, and temporary checkout cleanup.
- [x] Build a source-file ownership lookup during registration, explicitly marking input files as project-owned and library files with their dependency name. After registry parsing, derive object ownership keyed by normalized object type and name.
- [x] Keep ABAP registry filenames and object names unchanged. Grouping must not change ABAP name resolution or imply support for duplicate ABAP objects.
- [x] Verify existing project/dependency overlap and duplicate-object behavior. Preserve project precedence where it exists and report ambiguous dependency ownership rather than guessing.
- [x] Retain ownership through `build()` to generation and writing; classify outputs using `output.object` rather than filename guesses.

### 3. Introduce one output path resolver

- [x] Add an optional output layout context to the transpiler, supplied by the CLI. Existing callers without it, including `runRaw()` and the web playground, retain flat output.
- [x] Implement shared helpers to resolve an object's output path (`project/<filename>` or `<dependency-name>/<filename>`) and compute an import specifier relative to the importing module. Shared entry points resolve directly to the root.
- [x] Apply ownership to every `IOutputFile`, including multiple outputs per object and plugin outputs. Define `filename` consistently as relative to the output root when a layout is supplied.
- [x] Separate filesystem joining from import generation: imports use forward slashes and a leading `./` or `../`.
- [x] Escape URL-special characters per path segment while preserving directory separators. Do not apply namespace slash escaping to a whole output path.

### 4. Generate imports using the layout

- [x] Update `HandleABAP.addImportsAndExports()` to resolve required modules from the current module, preserving merged class-local imports and self-import detection.
- [x] Cover project-to-project, project-to-dependency, same-dependency, dependency-to-project, and dependency-to-dependency references. For example, `project/zapp.prog.mjs` imports a library using `../open-abap-core/<module>.mjs`, while two project modules use `./<module>.mjs`.
- [x] Update both initialization scripts in `initialization.ts` to import project, dependency, and plugin modules from resolved folders while preserving initialization and class-constructor ordering.
- [x] Update the CLI's program bootstrap to locate the shared root `_init.mjs` from the program's directory. Both project and dependency programs use `../_init.mjs` in this one-level layout.
- [x] Route unit-test paths through the same resolver: root runners import `./project/<class>.clas.testclasses.mjs`, and test classes resolve local/helper modules within `project/`. Keep dependency tests excluded and both runners in the root.
- [x] Generate correct specifiers at their original generation sites so `Chunk` source mappings remain valid.

### 5. Write nested files and preserve assets

- [x] Update `FileOperations.writeFiles()` to create parent directories recursively; create the configured output root recursively too.
- [x] Write resolved paths beneath the configured output directory and detect conflicting destinations before writing.
- [x] Keep MIME/Web Repository binary encoding unchanged and place data beside its owning project or dependency module.
- [x] Update W3MI/SMIM registered asset filenames to the emitted data paths. Inspect consumers in runtime/extras and verify loading from the output root.
- [x] Keep shared bootstrap and unit-test files at the root; do not duplicate runtime initialization into project or dependency folders.

### 6. Adjust source maps for nested output

- [x] Write each `.map` beside its module; `sourceMappingURL` must reference the escaped map basename, without repeating the project or dependency directory.
- [x] Compute source paths relative to the actual map directory for project and local dependency sources. Preserve forward slashes, escaping, and the program bootstrap's one-line offset.
- [x] Pass library source metadata to map generation; it currently receives only project files.
- [x] For cloned dependencies, embed source content in maps or persist referenced sources so maps remain usable after checkout cleanup. Prefer embedding to avoid another output directory hierarchy.

### 7. Add focused regression coverage

- [x] Test explicit names, HTTPS/SSH URLs, trailing `.git`/slashes, folder-only libraries, invalid names, duplicates, and case-insensitive collisions with reserved `project`.
- [x] Add an offline CLI integration fixture with a project and two local libraries. Assert all project files are under `project/`, each dependency has its named folder, and only shared generated entry points remain at the root. Execute generated code to verify references across folders and both initialization entry points.
- [x] Exercise a dependency superclass, merged class locals, a function group, and project unit tests to cover execution and initialization order.
- [x] Verify plugin ownership and binary MIME asset loading, including an exact byte comparison.
- [x] Verify nested source maps, namespace/percent characters, program line offsets, and usable cloned-dependency sources after checkout removal.
- [x] Cover no-library builds (still emitting into `project/`), multiple input folders sharing `project/`, direct execution of `project/zapp.prog.mjs`, source maps disabled, repeated builds, and unchanged flat behavior for direct transpiler callers without a layout.

### 8. Validate and document the change

- [x] Run focused tests, then root `npm test` for compilation, the existing suite, and linting. Verify the CLI webpack build and regenerated schema.
- [x] Document the fixed `project/` folder, combined ownership for multiple input folders, reserved-name rule, `libs[].name`, and the directory tree in `packages/cli/README.md`.
- [x] Document migration: project module/asset paths move into `project/`, dependency paths move into named folders, and shared entry points retain their locations. Update direct-run scripts from `node output/zapp.prog.mjs` to `node output/project/zapp.prog.mjs`; `node output/index.mjs` stays the same. Recommend rebuilding a clean generated output directory to remove stale flat project/dependency files, without deleting unrelated user files automatically.

## Completion criteria

Every emitted project file is under `project/` and every dependency file is under its resolved dependency name; shared entry points remain at the root. Imports, tests, MIME assets, and source maps work on Windows and POSIX. Existing configurations use `project/` and derive dependency names automatically, and direct transpiler consumers retain flat behavior unless they supply a layout.

## Implementation and validation

Implemented the project/dependency layout, optional library names, ownership tracking, relative imports, nested writing, MIME paths, and embedded source maps. Generic `package.devc.xml` metadata keeps the registry's existing last-copy behavior; runtime object collisions across libraries are rejected.

- Added 16 focused layout and CLI regression tests.
- Compilation for all packages and the CLI webpack build passed.
- Regenerated the CLI schema (the repository ignores this generated file).
- Repository lint passed.
- Full `npm test` reached the existing `T000 populate` test, which failed because its GitHub fetch was unavailable.
- The full suite excluding that network-dependent test passed: 2,692 passing, 30 pending. Command: `node node_modules/mocha/bin/mocha.js --timeout 60000 --grep "T000 populate" --invert`.
- Validation ran on Windows; import paths use POSIX separators on every platform.
