import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {SourceTranspiler} from "../expressions";
import {UniqueIdentifier} from "../unique_identifier";
import {Traversal} from "../traversal";

export class DoTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    const found = node.findFirstExpression(abaplint.Expressions.Source);
    if (found) {
      const source = new SourceTranspiler().transpile(found, traversal) + ".get()";
      const idSource = UniqueIdentifier.get();
      const id = UniqueIdentifier.get();
      return `const ${idSource} = ${source};
for (let ${id} = 0; ${id} < ${idSource}; ${id}++) {
abap.builtin.sy.get().index.set(${id} + 1);`;
    } else {
      return "while (true) {";
    }
  }

}