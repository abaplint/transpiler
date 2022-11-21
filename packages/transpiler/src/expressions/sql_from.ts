import {Nodes} from "@abaplint/core";
import * as abaplint from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class SQLFromTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, _traversal: Traversal): Chunk {
    const chunk = new Chunk();

    for (const c of node.getChildren()) {
      if (c instanceof abaplint.Nodes.TokenNode) {
        chunk.appendString(c.concatTokens() + " ");
      } else {
        chunk.appendString(c.concatTokens() + " ");
      }
    }

    return chunk;
  }

}