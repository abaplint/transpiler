import {Nodes} from "@abaplint/core";
import * as abaplint from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {SQLCondTranspiler} from "./sql_cond";
import {SQLFromSourceTranspiler} from "./sql_from_source";

export class SQLJoinTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const chunk = new Chunk();

    for (const c of node.getChildren()) {
      if (c instanceof abaplint.Nodes.TokenNode) {
        // keywords
        chunk.appendString(c.concatTokens() + " ");
      } else if (c.get() instanceof abaplint.Expressions.SQLCond) {
        chunk.appendChunk(new SQLCondTranspiler().transpile(c, traversal));
      } else if (c.get() instanceof abaplint.Expressions.SQLFromSource) {
        chunk.appendChunk(new SQLFromSourceTranspiler().transpile(c, traversal));
      } else {
        chunk.appendString(c.concatTokens() + " ");
      }
    }

    return chunk;
  }

}