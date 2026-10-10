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

The motivating generated file is not present in this checkout. Treat its reported
size as context; obtain its input revision and build configuration for measurement.

## Design

### 1. Share constructor code within each final generated module

Add a `TypeFactoryRegistry` owned by a final output module. Thread it explicitly
through `Traversal` and direct type-producing handlers. Do not use a process-global
registry or mutable static current context.

Initially extract concrete structures, tables, and data references whose targets
contain composite types. Keep small elementary constructors inline. Intern nested
composites too, so repeating a child in different parent types reuses its factory.
Start with one factory for every eligible distinct composite, including types used
once; measure the declaration/call overhead on small modules before default rollout.
If that overhead is material, add a measured extraction policy in a follow-up rather
than an arbitrary size threshold.

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

Introduce a constructor descriptor shared by identity calculation and rendering,
so they cannot disagree about which properties affect generated values. A descriptor
contains a constructor kind, its emitted arguments/metadata, and child descriptors.

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

Intern bottom-up using shallow descriptor keys containing interned child IDs. Use
exact key equality; a hash alone must never establish equivalence. This avoids
building a fully expanded constructor string just to deduplicate it. Memoize type
object resolution with a registry-local `WeakMap`, separating any options that
change rendering. Equivalent types from separate abaplint objects must still intern
to the same descriptor.

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
- Descriptors expose unsupported/error status recursively. Update
  `declareStaticSkipVoid()` to inspect that status before registering/emitting a
  creation expression, rather than searching generated JavaScript.
- Keep unknown/void/unsupported graphs on the existing inline error path initially.
  Preserve compile-error versus runtime-error configuration and lazy DDIC errors.
- Detect cycles while building descriptors. Fall back to the existing supported
  emission path where possible; otherwise report a bounded diagnostic instead of
  recursing indefinitely. This feature does not introduce recursive ABAP type support.

### 4. Integrate with module assembly

Add registry-aware type creation methods to `Traversal` and migrate callers of
`toType()`, `declare()`, and `declareStaticSkipVoid()` to them. Retain an inline path
for disabled mode and direct utility callers. Direct handlers receive an explicit
registry and use the same descriptor/emitter implementation.

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

Complete the steps in order. Mark each checkbox when its work and verification
are complete; all implementation tasks are initially unchecked.

### Step 1: Record a baseline

- [ ] Add a reproducible synthetic ABAP fixture with a wide, deeply nested structure
  reused in declarations, parameters, attributes, table rows, and expressions.
- [ ] Record UTF-8 bytes, repeated constructor counts, transpilation time, parse
  time, startup time, and construction throughput before implementation.
- [ ] Obtain the real motivating input revision and build configuration when
  available, and record its baseline separately.

### Step 2: Separate descriptors from inline rendering

- [ ] Introduce constructor descriptors and refactor `transpile_types.ts` to render
  them without changing generated output.
- [ ] Cover metadata distinctions, component order, and effective constructor
  options, including `packedDecimals` overrides.
- [ ] Add recursive unsupported status and update `declareStaticSkipVoid()` to
  inspect it while preserving existing output and error timing.

### Step 3: Implement the registry and fresh factories

- [ ] Add `packages/transpiler/src/type_factory_registry.ts` with module-local
  ownership and bottom-up descriptor interning.
- [ ] Add registry-local memoization, exact identity comparisons, and cycle handling.
- [ ] Implement deterministic, collision-safe helper names that remain distinct
  when separate generated chunks are concatenated.
- [ ] Emit factory declarations and child factory calls that construct fresh values;
  preserve `toTypeFunction()`'s existing lazy singleton wrapper.

### Step 4: Connect all generation paths

- [ ] Add registry-aware creation methods to `Traversal` and migrate `inline.ts`,
  expression emitters, and statement emitters.
- [ ] Update `HandleABAP` so merged class locals share one registry and separate
  final modules have independent registries.
- [ ] Update `HandleFUGR` to share a registry across its sequenced source files.
- [ ] Update DDIC and type-pool handlers and audit remaining `TranspileTypes` calls
  and handwritten constructor emitters.
- [ ] Assemble one factory prelude per final module after merging chunks, then
  apply indentation and import/export wrapping with correct source-map offsets.

### Step 5: Introduce an opt-in switch

- [ ] Add `sharedTypeFactories?: boolean` to `ITranspilerOptions`, initially
  defaulting to false for baseline comparison.
- [ ] Preserve the existing inline emission path when the option is disabled.
- [ ] Document the option in `packages/transpiler/README.md` and expose it through
  the CLI's existing `options` configuration/schema flow.

### Step 6: Validate and measure

- [ ] Add emission, registry, and end-to-end tests covering the validation checklist below.
- [ ] Run enabled/disabled comparisons and update output assertions only where
  the intended generated representation changes.
- [ ] Add `performance/type_factories.ts` and a dedicated package script.
- [ ] Complete the performance measurement checklist below and document benchmark
  results, including small-module and runtime tradeoffs.
- [ ] Run root `npm test` to compile and execute the full Mocha suite and lint.

### Step 7: Roll out

- [ ] Verify every acceptance criterion below and resolve material regressions.
- [ ] Enable shared factories by default, retaining `sharedTypeFactories: false`
  as a compatibility and diagnostic escape hatch.
- [ ] Update documentation for the default and rerun the required checks after
  changing it.

## Validation

Add `packages/transpiler/test/shared_type_factories.ts` for emission and registry
behavior, and end-to-end tests alongside `test/types/structure.ts`,
`test/types/data_reference.ts`, and the relevant expression/statement tests.

Required coverage:

- [ ] Repeated nested types emit one definition per eligible descriptor; repeated child
  types shared by distinct parents also emit once. Separate abaplint instances with
  equivalent semantics deduplicate.
- [ ] Same-shaped types with different metadata, field order, lengths, decimals, include
  suffixes, table options, or reference metadata remain distinct where required.
- [ ] Mutating nested fields, appending rows, changing row templates, assigning references,
  and clearing one value never affects another factory result. Include aliasing works
  within each instance. DDIC singleton behavior remains unchanged.
- [ ] Declarations, optional/returning parameters, inline data, class/interface attributes
  and types, `VALUE`, `NEW`, `CREATE DATA`, `CAST`, `CORRESPONDING`, and field symbols
  use the intended creation path and preserve observable ABAP results.
- [ ] Merged class locals share definitions without duplicate declarations. Function groups
  share definitions across source files. Separate modules and concatenated chunks do
  not collide. Helpers are available for top-level/static initialization.
- [ ] Unknown/void types preserve compile/runtime behavior, including nested unsupported
  types skipped by static type declarations and deferred DDIC errors.
- [ ] Output is identical across repeated runs and unaffected by unrelated registry objects.
  Interleaved independent runs cannot leak factory state. Disabled mode preserves
  existing output.
- [ ] Source-map lookups still resolve statements after the helper prelude, class-local
  merging, indentation, CommonJS-option import wrapping, and CLI program preambles.
  `ignoreSourceMap` remains effective.
- [ ] Browser playground execution, raw chunks, flat/grouped CLI output, and existing
  runtime versions work without helper imports or new runtime exports.

Run focused tests during implementation, then root `npm test` (compile, full Mocha
suite, and lint) before default rollout. Use existing source-map and output-layout
tests as regression coverage.

## Performance measurements and acceptance criteria

Add `performance/type_factories.ts` and a dedicated package script. Compare enabled
and disabled output from the same source revision, options, dependencies, Node
version, and machine. Use warmups and multiple runs; report medians and spread.

Measure independently:

- [ ] Generated UTF-8 bytes for the large class and the total output; helper declarations
  count toward the total. Minified bytes are a secondary metric with a pinned tool.
- [ ] Transpilation wall time, CPU time, and memory. Separate unchanged ABAP parsing and
  validation from type rendering where instrumentation permits.
- [ ] JavaScript compilation/parsing of identical generated inputs using Node's VM module
  compilation facilities, without evaluating ABAP constructors. Keep this separate
  from cold module import/evaluation, which includes initialization and I/O. Use fresh
  processes for cold imports to avoid the module cache.
- [ ] Repeated fresh-value construction and representative existing runtime workloads,
  to detect factory-call overhead independently of startup improvements.

Done means:

- [ ] Every eligible repeated composite is represented by one factory per final module,
  with all of its construction sites calling that factory.
- [ ] Enabled and disabled modes have equivalent observable ABAP behavior and metadata;
  mutation isolation, lazy errors, module assembly, and source-map regressions pass.
- [ ] The synthetic repetition fixture is at least 50% smaller before minification,
  including its factories. Measure the real 7.9 MB example and report its reduction
  when the input is available; do not claim a verified result without that fixture.
- [ ] Small-module output growth and runtime construction costs are reported. Target no
  more than a 5% median slowdown in representative transpilation/runtime workloads;
  investigate changes beyond measurement noise before enabling by default.
- [ ] Parsing/startup results are recorded separately. A parsing-time gain is an expected
  benefit to verify, not a prerequisite asserted from byte reduction alone.

## Deferred work

Cross-module factory libraries, prototype caching/cloning, runtime type interning,
new recursive-type semantics, unrelated metadata fixes, and general minifier changes
are outside this implementation. Revisit cross-module sharing or extraction thresholds
only after measurements identify remaining duplication or material helper overhead.
