import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {CondTranspiler} from "../expressions";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class WhileTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const cond = new CondTranspiler().transpile(node.findFirstExpression(abaplint.Expressions.Cond)!, traversal);

    return new Chunk()
      .append("while (", node, traversal)
      .appendChunk(cond)
      .append(") {", node.getLastToken(), traversal);
  }

}