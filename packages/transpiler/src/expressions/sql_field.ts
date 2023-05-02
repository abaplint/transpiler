import * as abaplint from "@abaplint/core";
import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler.js";
import {Traversal} from "../traversal.js";
import {Chunk} from "../chunk.js";
import {SQLFieldNameTranspiler} from "./sql_field_name.js";

export class SQLFieldTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const chunk = new Chunk();

    for (const c of node.getChildren()) {
      if (c instanceof abaplint.Nodes.TokenNode) {
        // keywords
        chunk.appendString(c.concatTokens() + " ");
      } else if (c.get() instanceof abaplint.Expressions.SQLFieldName) {
        chunk.appendChunk(new SQLFieldNameTranspiler().transpile(c, traversal));
      } else {
        chunk.appendString(c.concatTokens() + " ");
      }
    }

    return chunk;
  }

}