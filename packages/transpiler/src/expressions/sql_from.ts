import {Nodes} from "@abaplint/core";
import * as abaplint from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler.js";
import {Traversal} from "../traversal.js";
import {Chunk} from "../chunk.js";
import {SQLJoinTranspiler} from "./sql_join.js";
import {SQLFromSourceTranspiler} from "./sql_from_source.js";

export class SQLFromTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const chunk = new Chunk();

    for (const c of node.getChildren()) {
      if (c instanceof abaplint.Nodes.TokenNode) {
        // keywords
        chunk.appendString(c.concatTokens() + " ");
      } else if (c.get() instanceof abaplint.Expressions.SQLJoin) {
        chunk.appendChunk(new SQLJoinTranspiler().transpile(c, traversal));
        chunk.appendString(" ");
      } else if (c.get() instanceof abaplint.Expressions.SQLFromSource) {
        chunk.appendChunk(new SQLFromSourceTranspiler().transpile(c, traversal));
      } else {
        if (c.findFirstExpression(abaplint.Expressions.Dynamic)) {
          chunk.appendString(c.concatTokens() + " ");
        } else if (c.concatTokens().includes("/")) {
          chunk.appendString("'" + c.concatTokens() + "' ");
        } else {
          chunk.appendString(c.concatTokens() + " ");
        }
      }
    }

    return chunk;
  }

}