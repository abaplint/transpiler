import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {CondTranspiler} from "../expressions";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {UniqueIdentifier} from "../unique_identifier";

export class WhileTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const cond = new CondTranspiler().transpile(node.findFirstExpression(abaplint.Expressions.Cond)!, traversal);
    const unique = UniqueIdentifier.get();

    return new Chunk()
      .append(`let ${unique} = 1;
while (`, node, traversal)
      .appendChunk(cond)
      .appendString(`) {
abap.builtin.sy.get().index.set(${unique}++);`);
  }

}