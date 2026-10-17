# @abaplint/transpiler

Transpiler

The transpiler emits shared factory functions by default for repeated composite
structure, table-row, and data-reference constructors within each generated module.
Each call creates a fresh runtime value. Set `sharedTypeFactories: false` in
`ITranspilerOptions` to preserve inline constructor output.
