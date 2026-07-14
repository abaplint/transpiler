# Plan: Fix Source Map Emission

## Current state / root causes found

### A. The CLI never writes source maps at all
[packages/cli/src/index.ts:82-85](packages/cli/src/index.ts#L82-L85) chains the object-type
checks with `&&`:

```ts
if (writeSourceMaps === true
    && output.object.type.toUpperCase() === "PROG"
    && output.object.type.toUpperCase() === "FUGR"
    && output.object.type.toUpperCase() === "CLAS") {
```

An object cannot be PROG *and* FUGR *and* CLAS, so the condition is always false.
Should be `||` around the three type checks.

### B. PROG output shifts all lines by one *after* the map is generated
[packages/cli/src/index.ts:103-106](packages/cli/src/index.ts#L103-L106) prepends
`if (!globalThis.abap) await import("./_init.mjs");\n` to PROG contents **after** the
map has been generated → every mapping in a PROG is off by one generated line.
The same ordering concern applies to anything appended/prepended outside of `Chunk`
(the `sourceMappingURL` comment is fine since it's last).

### C. `Chunk.appendChunk` has two correctness bugs
[packages/transpiler/src/chunk.ts:37-58](packages/transpiler/src/chunk.ts#L37-L58)

1. **Missing line shift.** When an appended mapping is on generated line 1 and
   `this.raw` does not end with `\n`, only the column is shifted — the line is left
   at 1. If `this.raw` is multi-line (e.g. `"a\nb"`), the mapping should land on the
   last line of the buffer, not line 1. The branch logic collapses to:

   ```ts
   if (m.generated.line === 1) {
     add.generated.column += lastLine.length; // lastLine is "" when raw ends with \n
   }
   add.generated.line += lineCount - 1;       // always
   ```

2. **In-place mutation / aliasing.** `const add = m;` mutates the *source* chunk's
   mapping objects. Appending the same chunk twice, or keeping a reference to a
   sub-chunk, double-shifts positions. Mappings must be deep-copied
   (`{source, generated: {...}, original: {...}}`) before adjusting.
   This is likely why `clear.ts` currently works around `appendChunk(target)` by
   using `target.getCode()` string concatenation instead.

### D. Statement transpilers are inconsistent and mostly unmapped
- Only **69 of 165** statement transpilers call `Chunk.append()` with a position at
  all; the rest build plain string chunks → zero mappings for those statements.
- Of those that do, the position argument is inconsistent:
  - 59 × `node` (→ first token start — the desirable convention)
  - 36 × `node.getLastToken()` (→ points at the final `.` of the statement)
  - 3 × `node.getLastToken().getEnd()` (→ points *past* the statement, e.g. `clear.ts`)
- Sub-expression chunks are frequently flattened via `chunk.getCode()` string
  concatenation, which throws away the expression-level mappings the traversal
  already produced.

### E. Minor / hygiene
- `runIndentationLogic` ([chunk.ts:116-151](packages/transpiler/src/chunk.ts#L116-L151))
  can apply a negative shift if brace counting goes below zero; column shift should be
  clamped and only applied when the line was actually indented.
- `getMap` emits no `sourcesContent`; the CLI patches source paths afterwards via
  string replace ([index.ts:90-99](packages/cli/src/index.ts#L90-L99)) — fragile,
  should be an input to `getMap` instead.
- `stripLastNewline` doesn't drop mappings that pointed at the removed newline (low impact).

---

## Fix strategy

Order matters: fix the foundation (`Chunk`), then emission (CLI), then converge the
~165 statement transpilers on one convention, with automated validation so
regressions can't creep back in. CLEAR becomes the reference implementation.

**Target convention for statement transpilers:**
1. The first `append()` of a statement uses `node.getFirstToken().getStart()` so the
   generated statement maps to the start of the ABAP statement (what a debugger needs
   for breakpoints and stepping).
2. Sub-chunks from `traversal.traverse(...)` are added with `appendChunk()` (never
   flattened with `getCode()`), preserving expression-level mappings.
3. Trailing syntax (`;`, `)`) is appended with the last token's position, not `getEnd()`.

---

## Checklist

### Phase 1 — Chunk core fixes (packages/transpiler/src/chunk.ts)
- [ ] Fix `appendChunk` line-shift bug: always apply `generated.line += lineCount - 1`; apply column shift only for mappings on line 1 of the appended chunk
- [ ] Fix `appendChunk` aliasing: deep-copy mappings before adjusting so the source chunk is never mutated
- [ ] Add unit tests in `packages/transpiler/test/chunk.ts`: multi-line buffer without trailing newline, appending the same chunk twice, nested `appendChunk` of chunks that themselves contain mappings
- [ ] Guard `runIndentationLogic` against negative indent and add a mapping-adjustment test for indented + `}` lines

### Phase 2 — Emission fixes (packages/cli/src/index.ts)
- [ ] Fix the `&&` → `||` object-type condition so maps are actually written
- [ ] Move the `_init.mjs` prepend for PROG *before* map generation (or prepend via a `Chunk` so mappings shift correctly)
- [ ] Pass source paths (and optionally `sourcesContent`) into `Chunk.getMap()` instead of string-replacing the JSON afterwards
- [ ] Add a CLI-level test that a PROG/CLAS build with `write_source_map: true` produces a `.map` file whose first statement maps to the correct ABAP line

### Phase 3 — Convention + reference implementation
- [ ] Decide and document the convention (statement start = first token start; preserve sub-chunk mappings) in `design-notes.md`
- [ ] Rewrite `statements/clear.ts` as the reference: `appendChunk(target)` + `append(".clear();", node.getLastToken(), traversal)`; remove the commented-out experiments
- [ ] Add a source-map test for CLEAR in `packages/transpiler/test/source_map.ts` asserting both the statement mapping and the target expression mapping
- [ ] Add a reusable test helper that, given ABAP + generated JS + map, asserts every generated line with code has ≥ 1 mapping and all original positions fall inside the ABAP source

### Phase 4 — Migrate statement transpilers
- [ ] Fix the 36 `node.getLastToken()` and 3 `getLastToken().getEnd()` call sites to follow the convention
- [ ] Sweep the 69 mapped transpilers for `getCode()` flattening of traversed sub-chunks; replace with `appendChunk`
- [ ] Add mappings to the ~96 unmapped statement transpilers, prioritized by frequency in real code (MOVE/compute, IF, LOOP, CALL METHOD, WRITE, APPEND, READ TABLE, ...)
- [ ] Do the same pass over `expressions/` and `structures/` (structure transpilers emit the `{`/`}` scaffolding — map them to their opening/closing statements)

### Phase 5 — Validation & end-to-end verification
- [ ] Extend `source_map.ts` tests to cover a full method body mixing mapped statements, verifying line *and* column via `SourceMapConsumer.originalPositionFor`
- [ ] Build a small PROG via the CLI, run it in Node with `--enable-source-maps`, throw an error, and assert the stack trace shows the `.abap` file and correct line
- [ ] Manually verify VS Code debugging (breakpoints in `.abap`, stepping) against a real project, e.g. open-abap
- [ ] Run the full transpiler test suite and the downstream test suites (`unit-test/`, database tests) to confirm no behavior change in generated code

---

## Risks / notes

- Phase 1's `appendChunk` fix changes mapping output everywhere — the three existing
  tests in `source_map.ts` assert mapping *counts* per line, which may legitimately
  change. Update expectations deliberately, not mechanically.
- Generated *code* must not change in phases 1–2 (maps only). Phases 3–4 should also
  be code-neutral; keep `getCode()` output byte-identical while switching to
  `appendChunk`, so snapshot/behavior tests catch accidents.
- `rearrangeClassLocals` concatenates two files' chunks into one `.mjs`
  ([handle_abap.ts:105](packages/transpiler/src/handlers/handle_abap.ts#L105)); after
  the Phase 1 fix this should just work since mappings carry their own `source`
  filename — add a test to confirm.
