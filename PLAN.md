# Shared type factories

## Goal

Deduplicate generated ABAP type constructor expressions so large nested structures
are defined once and instantiated through short factory calls. The motivating
example is `zcl_gg_host_runtime.clas.mjs`, reported to be 7.9 MB before minification.
Reduce generated JavaScript size and measure whether this also reduces JavaScript
parsing and module startup time.

This is a code-generation change. Every normal construction must still return a
fresh runtime value with the same initial state and type metadata.

## Findings in the current implementation

- `packages/transpiler/src/transpile_types.ts` recursively expands structures,
  table row types, and data reference targets in `TranspileTypes.toType()`.
  Declarations, method parameters, class attributes, inline declarations, and
  constructor expressions all use this path.
- `toTypeFunction()` already creates a lazy singleton for DDIC type descriptions.
  Its existing lifetime and error timing are separate from ordinary value creation.
- `declareStaticSkipVoid()` currently detects unsupported types by searching the
  generated expression for error text. Replacing that expression with a factory
  call would hide the error unless this check is made structural.
- `HandleABAP` generates separate chunks and then merges class locals definitions
  and implementations. `HandleFUGR` combines several source files into one module.
  Factory ownership must follow the final output module, not each source file.
- `Chunk.appendChunk()` adjusts source mappings when combining chunks. Factories
  must enter this pipeline before final indentation and import/export wrapping.
- The browser playground and execution tests consume generated chunks directly,
  sometimes concatenating several chunks into one function. Generated helpers must
  work in these environments as well as in `.mjs` files.
- Runtime structures, tables, and references contain mutable state. In particular,
  `Table.clone()` retains the row type and `DataReference.clone()` retains the
  referenced type. A cached prototype followed by `.clone()` is not a safe general
  replacement for fresh construction.

The motivating generated file is not present in this checkout. Its source is
available at [`open-abap/open-abap-gui`, commit
`815d81225e7ff40d7e271f1771c197c6fae8d052`](https://github.com/open-abap/open-abap-gui/blob/815d81225e7ff40d7e271f1771c197c6fae8d052/framework/zcl_gg_host_runtime.clas.abap)
(60,884 source bytes). Its `package-lock.json` pins the CLI at 2.14.0 and
`abap_transpile.json` selects `src`, `framework`, and `examples`, plus three
upstream ABAP libraries. `abaplint.jsonc` identifies those libraries only by GitHub
URL without commit revisions, and the original generated `.mjs` file is not
archived. A target-module reconstruction using the source commit and explicit
dependency snapshots is recorded below: it produces 7,904,362 inline bytes with
this checkout's transpiler 2.14.3 and core 2.120.72. That matches the reported 7.9 MB
scale; the exact historical CLI 2.14.0 build is still unverified.

## Design

### 1. Share constructor code within each final generated module

Add a `TypeFactoryRegistry` owned by a final output module. Thread it explicitly
through `Traversal` and direct type-producing handlers. Do not use a process-global
registry or mutable static current context.

Extract concrete structures, table rows, and data references whose targets contain
composite types. Keep elementary constructors inline. Intern nested composites too,
so repeated children are shared by parent factories. At finalization, compare the
UTF-8 cost of each helper declaration and its calls with the inline expressions;
retain a helper only when it reduces output bytes. This measured policy avoids
arbitrary type-size thresholds and prevents small modules from growing.

Example, with illustrative helper names:

```javascript
function $type_zexample_1() {
  return new abap.types.Structure({
    "name": new abap.types.Character(30),
    "count": new abap.types.Integer()
  }, undefined, undefined, {}, {});
}

function $type_zexample_2() {
  return new abap.types.Structure({
    "header": $type_zexample_1(),
    "detail": $type_zexample_1()
  }, undefined, undefined, {}, {});
}

let first = $type_zexample_2();
let second = $type_zexample_2();
```

Factories contain only constructor expressions and calls to other factories. They
do not read local ABAP variables, resolve types through `abap.DDIC`, import ABAP
classes, initialize cached values, or execute at module load merely because they
are declared. Continue to construct tables through `abap.types.TableFactory.construct()`.

Module-local sharing directly addresses repetition inside the reported large class
and preserves self-contained output. Cross-module factory libraries are deferred:
they would require additional output artifacts, layout/import handling, and a policy
for dependency ownership. Measure remaining cross-module duplication before adding
that scope.

### 2. Intern by complete constructor semantics

Use the fully emitted constructor expression as the exact intern key. Since the
renderer produces the key directly, identity includes every argument and metadata
field that currently affects generated values without a second descriptor serializer.
Do not use a hash alone to establish equivalence.

Identity must account for:

| Type | Required distinctions |
| --- | --- |
| Elementary | Runtime constructor kind, length, effective decimals, and all emitted abstract type metadata |
| Structure | Ordered component names and child identities, emitted qualified/DDIC names, include flags, and renaming suffixes |
| Table | Row identity, qualified name, header behavior, primary/secondary keys, uniqueness, access kind, and every emitted table option |
| Data reference | Target identity and current constructor semantics |
| Object reference | Qualified name, RTTI name, and existing definition-based metadata recovery |

Preserve component order: runtime structure assignment uses that order. Canonicalize
metadata object keys only where order has no behavior; preserve ordered key lists
and component lists. Treat missing values distinctly wherever rendering does.
Use effective constructor arguments rather than ABAP names alone, object identity,
or a lossy textual type description. Include `packedDecimals` overrides in the
descriptor/memoization key when applicable.

Intern bottom-up using exact emitted expressions containing child factory calls.
The keys stay shallow because child composites are already interned. Memoize type
object resolution with a registry-local `WeakMap`, separating any options that
change rendering. Equivalent types from separate abaplint objects produce the same
constructor expression and therefore the same intern key.

Audit the existing structure DDIC-name argument, which currently uses the qualified
name when `getDDICName()` is present. Preserve current emitted behavior here; handle
any metadata correction as a separate change with its own tests.

### 3. Preserve construction and failure behavior

- Every factory invocation allocates a fresh structure, all of its fields, tables,
  row templates, table headers, reference wrappers, and reference target templates.
  Factory calls never return a cached value or clone a shared prototype.
- Include aliases must point into their own freshly constructed include. Sharing
  helper code must not introduce aliases between independent ABAP values.
- Keep `toTypeFunction()`'s lazy singleton wrapper around the new creation expression.
  DDIC consumers continue to observe its existing identity and initialization timing.
- Class/interface static type prototypes remain ordinary independent constructions.
  Existing constant/default initialization stays at the call site after construction.
- Inspect unsupported/error status recursively before registering or emitting a
  creation expression, rather than searching generated JavaScript.
- Keep unknown/void/unsupported graphs on the existing inline error path initially.
  Preserve compile-error versus runtime-error configuration and lazy DDIC errors.
- Detect cycles during registry-backed type resolution. Fall back to the existing
  supported emission path where possible; otherwise report a bounded diagnostic
  instead of recursing indefinitely. This feature does not introduce recursive ABAP
  type support.

### 4. Integrate with module assembly

Add registry-aware type creation methods to `Traversal` and migrate callers of
`toType()`, `declare()`, and `declareStaticSkipVoid()` to them. Retain an inline path
for disabled mode and direct utility callers. Direct handlers receive an explicit
registry and use the same exact-expression interning implementation.

- In `HandleABAP`, plan final module ownership before traversing source files. Main,
  macros, and other separate modules have separate registries; locals definition
  and implementation share the registry for their merged `.clas.locals.mjs`.
- In `HandleFUGR`, share one registry across every sequenced source file contributing
  to the combined function-group module.
- Give DDIC and type-pool handlers a registry for each output module. Audit all
  `TranspileTypes` call sites so hot paths cannot silently retain expanded output.
- Audit handwritten constructor emitters such as enum structures. Route them through
  the registry where their semantics fit; leave fixed small unit-test/exception
  helpers outside the initial extraction scope.
- Emit helper function declarations once, after traversal has collected the graph
  and class locals have been merged. Prepend them using
  `new Chunk().appendChunk(factoryChunk).appendChunk(bodyChunk)`, then indent the
  assembled body once and apply existing import/export wrapping.
- Use deterministic helper names with a collision-safe reserved prefix and a stable,
  escaped final-module identifier. Local numbering must not depend on other modules,
  previous runs, or the global `UniqueIdentifier` counter. Check against emitted
  user identifiers. Names from distinct modules must remain distinct when chunks
  are concatenated by tests or consumers; do not rely solely on ESM lexical isolation.
- Keep call-site source mappings attached to their ABAP statements. Generated helper
  bodies may remain unmapped; do not assign all factory callers to the first type use.
  Test mapping shifts through factories, indentation, imports, and CLI preambles.

No new runtime API, generated auxiliary file, or output-layout convention is needed
for the initial module-local implementation.

## Implementation sequence

Complete the steps in order and keep each checkbox synchronized with completed
work and its verification.

### Step 1: Record a baseline

- [x] Add a reproducible synthetic ABAP fixture with wide nested structures reused
  across many declarations; include repeated composite table rows in end-to-end tests.
- [x] Record inline/shared output bytes, helper count, parse time, transpilation
  wall and CPU time, JavaScript compile time, and construction time on the same
  synthetic input and runtime.
- [x] Add representative class-attribute, optional/returning-parameter, and
  `VALUE`/`CORRESPONDING` expression-use cases to the benchmark fixture.
- [x] Measure cold module startup and minified output for the synthetic fixture
  with pinned Terser 5.51.2 and a fresh-process import harness.
- [x] Locate the real motivating input revision and build configuration:
  `open-abap/open-abap-gui` at `815d81225e7ff40d7e271f1771c197c6fae8d052`,
  `framework/zcl_gg_host_runtime.clas.abap`, configured by `abap_transpile.json`.
- [x] Reproduce the target class module using the upstream source revision and
  explicit ABAP dependency commit snapshots; record inline and shared baselines in
  the performance section below.
- [ ] Reproduce the exact historical build with upstream CLI 2.14.0 and its original
  dependency revisions. The generated artifact is not archived and the ABAP library
  URLs in the upstream configuration do not pin commits; this reconstruction uses
  the checkout's CLI 2.14.3 and core 2.120.72.

### Step 2: Define constructor identity and preserve inline rendering

- [x] Use the exact emitted constructor expression as the intern key, with child
  composites interned first. This keeps identity and rendered arguments aligned
  without a separate descriptor object or hash-only comparison.
- [x] Memoize type-object resolution per registry and rendering options, including
  `packedDecimals`, and report recursive type graphs with a bounded diagnostic.
- [x] Make `declareStaticSkipVoid()` inspect nested unsupported types structurally
  before registering constructors.
- [x] Expand identity and runtime tests across table options, include aliases and
  suffixes, composite-reference target metadata, and equivalent type objects
  constructed independently by abaplint.

### Step 3: Implement the registry and fresh factories

- [x] Add a module-local registry that interns exact constructor expressions
  bottom-up, including child factory calls.
- [x] Add registry-local memoization, exact identity comparisons, and bounded
  cycle handling.
- [x] Generate deterministic, source-collision-aware names that remain distinct
  when separate modules are concatenated.
- [x] Emit factory declarations and child factory calls that construct fresh values;
  preserve `toTypeFunction()`'s existing lazy singleton wrapper.
- [x] Inline candidates when declarations and calls would cost more bytes than the
  use-site expressions, so small modules do not grow.

### Step 4: Connect all generation paths

- [x] Add registry-aware creation methods to `Traversal` and migrate `inline.ts`,
  expression emitters, and statement emitters.
- [x] Update `HandleABAP` so merged class locals share one registry and separate
  final modules have independent registries.
- [x] Update `HandleFUGR` to share a registry across its sequenced source files.
- [x] Update DDIC and type-pool handlers and audit remaining `TranspileTypes` calls
  and handwritten constructor emitters.
- [x] Assemble one factory prelude per final module after merging chunks, then
  apply indentation and import/export wrapping with adjusted source-map offsets.

### Step 5: Introduce an opt-in switch

- [x] Add `sharedTypeFactories?: boolean` to `ITranspilerOptions`, initially
  defaulting to false for baseline comparison.
- [x] Preserve the existing inline emission path when the option is disabled.
- [x] Document the option in `packages/transpiler/README.md` and expose it through
  the CLI's existing `options` configuration/schema flow.

### Step 6: Validate and measure

- [x] Add emission, registry, runtime-isolation, source-map, and function-group tests
  for the behaviors covered below.
- [x] Run enabled/disabled comparisons and update output assertions only where
  the intended generated representation changes.
- [x] Add `performance/type_factories.ts` and a dedicated package script.
- [x] Run the synthetic enabled/disabled benchmark and record the measured results
  below, including cold imports and minified output. The target class reconstruction
  is also measured below; the exact historical CLI build remains pending.
- [x] Run root `npm test` after linking local packages: compilation, webpack, all
  tests, and lint pass with factories enabled by default (2,826 passing, 30 pending).
  CLI integration fixtures verify default, explicit-enabled, and explicit-disabled
  output in grouped and flat layouts.

### Step 7: Roll out

- [ ] Verify every acceptance criterion below and resolve material regressions. The
  browser playground UI remains unavailable for end-to-end execution in this session.
- [x] Enable shared factories by default, retaining `sharedTypeFactories: false`
  as a compatibility and diagnostic escape hatch.
- [x] Update transpiler and CLI documentation for the default and rerun the required
  checks after changing it: root `npm test` passes (2,826 passing, 30 pending).

## Validation

Add `packages/transpiler/test/shared_type_factories.ts` for emission and registry
behavior, and end-to-end tests alongside `test/types/structure.ts`,
`test/types/data_reference.ts`, and the relevant expression/statement tests.

Required coverage:

- [x] Repeated nested structures and composite table rows share factories; function
  groups share a repeated type across source files. Separate module names remain
  distinct when generated chunks are concatenated.
- [x] Different field order and emitted elementary length metadata produce distinct
  constructors.
- [x] Add explicit distinctions for table options and composite-reference
  metadata; exercise include aliases and suffixes at runtime, and equivalent
  type objects constructed independently by abaplint.
- [x] Runtime tests show independent nested structures, table rows, and composite
  data-reference wrappers.
- [x] Extend runtime coverage to include include aliasing, independent table
  headers and rows, clearing one value, and DDIC singleton timing.
- [x] Opt-in execution covers declarations, composite table rows, and `CREATE DATA`.
- [x] Run the full existing suite after default rollout; the root suite passes with
  local packages linked and factories enabled by default. Feature-specific cases
  explicitly cover `sharedTypeFactories: false` and compare it with factory mode.
- [x] Exercise optional/returning parameters, inline data, class/interface
  references and attributes, `VALUE`, `NEW`, `CAST`, `CORRESPONDING`, and field
  symbols with factories enabled; compare runtime results with legacy mode.
- [x] Function groups share definitions across source files, and concatenated modules
  have distinct helper names.
- [x] Test merged class locals, helper ordering before static initialization,
  nested unsupported-type behavior, and lazy DDIC singleton timing with factories
  enabled.
- [x] Repeated runs are deterministic; disabled mode emits no helpers and a small
  fixture matches legacy output byte for byte.
- [x] Source-map lookups remain valid with retained helpers, indentation, and
  CommonJS import/export wrapping.
- [x] Exercise source maps through class-local merging and the CLI preamble, and
  verify `ignoreSourceMap` remains effective with factories enabled.
- [x] Verify factories are emitted in raw chunks with no helper imports, and
  exercise flat and grouped CLI artifact collection in-process; generated code
  executes in the Node runtime tests without new runtime exports.
- [x] Exercise full CLI child-process execution in grouped mode with factories
  enabled, source maps, runtime setup, tests, and binary assets.
- [x] Exercise flat CLI child-process output with shared factories enabled; flat and
  grouped artifact collection are also covered in-process.
- [x] Enable factories in the browser playground and build its webpack bundle.
- [ ] Execute the browser playground UI end to end. The webpack build succeeds, but
  no interactive browser surface is available in this session: CUA has no IAB or
  Chrome browser, the Windows computer-use native pipe is unavailable, and local
  Chrome/Edge headless launches fail with Windows access-denied errors.

Run focused tests during implementation, then root `npm test` (compile, full Mocha
suite, and lint) before default rollout. Use existing source-map and output-layout
tests as regression coverage.

## Performance measurements and acceptance criteria

Add `performance/type_factories.ts` and a dedicated package script. Compare enabled
and disabled output from the same source revision, options, dependencies, Node
version, and machine. Use warmups and multiple runs; report medians and spread.

Measure independently:

- [x] Generated UTF-8 bytes and helper counts for the synthetic comparison, including
  helper declarations. The target class reconstruction is measured as well, and
  both synthetic and real-module minified sizes are recorded below.
- [x] Parse time, transpilation wall time, CPU time, and process RSS are reported
  separately by the harness.
- [x] JavaScript body compilation is measured with Node's AsyncFunction constructor,
  separately from runtime execution.
- [x] Synthetic cold module startup and minified output are measured with Terser
  5.51.2; cold startup includes a fresh Node process, runtime import, and generated
  ESM import.
- [x] Runtime execution of repeated fresh-value construction is measured on the
  synthetic fixture; execution of the real application workload remains unmeasured.

Latest synthetic comparison after the elementary-type fast path (2026-10-10, Node
v22.19.0, 20 repeated declarations plus class attributes, optional/returning
parameters, and `VALUE`/`CORRESPONDING` expressions; 2 warmups and 15 timed runs on
each mode):

| Measure | Inline (median; min-max) | Shared factories (median; min-max) |
| --- | ---: | ---: |
| Generated output | 3,770,435 bytes | 7,162 bytes |
| Factory definitions | 0 | 3 |
| Parse | 23.32 ms (13.58-79.20) | 22.18 ms (10.14-76.11) |
| Transpile | 256.42 ms (214.61-362.85) | 56.57 ms (44.87-87.77) |
| CPU | 328 ms (266-454) | 172 ms (78-250) |
| JavaScript compile | 4.03 ms (2.38-9.94) | 0.059 ms (0.049-0.082) |
| Runtime construction | 64.50 ms (47.09-101.79) | 12.57 ms (9.74-34.37) |
| RSS median | 146,878,464 bytes | 130,142,208 bytes |

The synthetic output reduction is 99.81%. Parse medians are close and their ranges
overlap, so this run does not establish a parsing-time improvement.
Minified output is 3,402,268 bytes inline and 3,578 bytes shared (99.89% smaller).
Fresh-process cold-start medians are 698.85 ms inline and 505.62 ms shared, including
runtime and generated-module imports; machine-load outliers remain visible in the
inline range (564.49-1,280.40 ms) versus 318.94-804.27 ms shared.

For the 81-byte small fixture, both modes emitted 138 bytes and no factories; both
minified to 115 bytes. With 15 timed runs, transpilation medians were 9.35 ms inline
and 7.90 ms shared (ranges 6.52-35.01 and 5.57-26.26 ms). Runtime construction
medians were 0.0405 and 0.0400 ms (ranges 0.0243-0.2100 and 0.0246-0.1976 ms).
The latest medians show no greater-than-5% slowdown; the broad, overlapping ranges
and occasional outliers still make tiny-fixture timing noisy. Tiny cold-start medians
were 339.28 ms inline and 321.94 ms shared.

The reported 7.9 MB class was reconstructed from its upstream ABAP source and
explicit dependency snapshots. The upstream source commit is
`815d81225e7ff40d7e271f1771c197c6fae8d052`; dependency commits selected at or
before that source revision are `open-abap-core`
`8b2ad62ca3a684fcddad4f6f9c7632dca66b06cc`, `express-icf-shim`
`996eb798ff8aa0a302c6672e95b1fde7b95bcbe3`, and `open-abap-bal`
`0a923be7debff210435af2432279ccf9be959195`. Since the upstream configuration
does not pin those libraries and the generated module is not archived, this is a
reproducible target-module reconstruction rather than a byte-for-byte reproduction
of the original historical build. Generation used this checkout's transpiler
2.14.3 and core 2.120.72, Node v22.19.0, and Terser 5.51.2. The target module had
no target-specific issues with syntax checks disabled; both generated modules pass
`node --check`. Loading the staged source subset through the current validator
reported 2,014 unrelated issues, so these numbers are from target-only generation,
not a clean full-project CLI build.

| Target class module measure | Inline | Shared factories |
| --- | ---: | ---: |
| Generated output | 7,904,362 bytes | 174,521 bytes |
| Factory definitions | 0 | 33 |
| Terser minified output | 7,066,270 bytes | 127,593 bytes |

The reconstructed class output is 97.79% smaller before minification and 98.19%
smaller after minification. This measures emitted size; runtime construction and
end-to-end application behavior for the class remain unmeasured.

Acceptance criteria:

- [x] Every repeated composite whose factory reduces emitted bytes is represented by
  one factory per final module; unprofitable candidates remain inline.
- [x] Confirm enabled and disabled runtime behavior across unsupported/lazy error
  paths, including preserved construction and error timing.
- [ ] Verify all CLI/browser assembly modes; grouped and flat CLI child-process
  output are covered, while browser playground UI execution remains open.
- [x] The synthetic repetition fixture is over 50% smaller before minification,
  including helper declarations (99.81% in this run).
- [x] Reconstruct and measure the reported 7.9 MB class module from the upstream
  source revision and explicit dependency snapshots: 7,904,362 inline bytes versus
  174,521 shared bytes (97.79% smaller), and 7,066,270 versus 127,593 minified
  bytes (98.19% smaller). The exact historical CLI 2.14.0 build remains unverified
  because the generated artifact and original library commit pins are unavailable.
- [x] Small-module byte and runtime results are reported. No output growth appeared
  for the tiny fixture, and the measured median changes are within the 5% target.
- [x] Recheck the 5% median slowdown target on tiny transpilation and runtime
  workloads before enabling by default. After the elementary-type fast path, the
  latest 15-run comparison shows no greater-than-5% median slowdown; timing remains
  machine-load-sensitive. The real application workload remains pending.
- [x] Parsing and generated-JavaScript compilation are recorded separately; a parsing
  gain remains an expected benefit, not a conclusion from byte reduction alone.

## Deferred work

Cross-module factory libraries, prototype caching/cloning, runtime type interning,
new recursive-type semantics, unrelated metadata fixes, and general minifier changes
are outside this implementation. Revisit cross-module sharing or extraction thresholds
only after measurements identify remaining duplication or material helper overhead.
