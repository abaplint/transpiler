import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler.js";
import {SourceTranspiler} from "../expressions/index.js";
import {UniqueIdentifier} from "../unique_identifier.js";
import {Traversal} from "../traversal.js";
import {Chunk} from "../chunk.js";

export class DoTranspiler implements IStatementTranspiler {
  private readonly syIndexBackup: string;

  public constructor(syIndexBackup: string) {
    this.syIndexBackup = syIndexBackup;
  }

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const found = node.findFirstExpression(abaplint.Expressions.Source);
    if (found) {
      const source = new SourceTranspiler(true).transpile(found, traversal).getCode();
      const idSource = UniqueIdentifier.get();
      const id = UniqueIdentifier.get();
      return new Chunk(`const ${this.syIndexBackup} = abap.builtin.sy.get().index.get();
const ${idSource} = ${source};
for (let ${id} = 0; ${id} < ${idSource}; ${id}++) {
abap.builtin.sy.get().index.set(${id} + 1);`);
    } else {
      const unique = UniqueIdentifier.get();
      return new Chunk(`const ${this.syIndexBackup} = abap.builtin.sy.get().index.get();
let ${unique} = 1;
while (true) {
abap.builtin.sy.get().index.set(${unique}++);`);
    }
  }

}