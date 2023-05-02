import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler.js";
import {Traversal} from "../traversal.js";
import {Chunk} from "../chunk.js";

export class ElseIfTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const cond = traversal.traverse(node.findFirstExpression(abaplint.Expressions.Cond));
    return new Chunk()
      .append("} else if (", node, traversal)
      .appendChunk(cond)
      .append(") {", node.getLastToken(), traversal);
  }

}