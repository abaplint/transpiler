import {Nodes} from "@abaplint/core";
import * as abaplint from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler.js";
import {Traversal} from "../traversal.js";
import {Chunk} from "../chunk.js";

export class SQLFromSourceTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, _traversal: Traversal): Chunk {
    const chunk = new Chunk();

    for (const c of node.getChildren()) {
      if (c instanceof abaplint.Nodes.TokenNode) {
        // keywords
        chunk.appendString(c.concatTokens() + " ");
      } else if (c.get() instanceof abaplint.Expressions.DatabaseTable && c.concatTokens().includes("/")) {
        chunk.appendString("'" + c.concatTokens() + "' ");
      } else {
        chunk.appendString(c.concatTokens() + " ");
      }
    }

    return chunk;
  }

}