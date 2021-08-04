import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {SourceTranspiler} from "../expressions";
import {UniqueIdentifier} from "../unique_identifier";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class DoTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const found = node.findFirstExpression(abaplint.Expressions.Source);
    if (found) {
      const source = new SourceTranspiler(true).transpile(found, traversal).getCode();
      const idSource = UniqueIdentifier.get();
      const id = UniqueIdentifier.get();
      return new Chunk(`const ${idSource} = ${source};
for (let ${id} = 0; ${id} < ${idSource}; ${id}++) {
abap.builtin.sy.get().index.set(${id} + 1);`);
    } else {
      const unique = UniqueIdentifier.get();
      return new Chunk(`let ${unique} = 1;
while (true) {
abap.builtin.sy.get().index.set(${unique}++);`);
    }
  }

}