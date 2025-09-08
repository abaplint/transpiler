import * as abaplint from "@abaplint/core";
import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {SQLFieldNameTranspiler} from "./sql_field_name";
import {FieldChainTranspiler} from "./field_chain";

export class SQLFieldTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const chunk = new Chunk();

    for (const c of node.getChildren()) {
      if (c instanceof abaplint.Nodes.TokenNode) {
        const concat = c.concatTokens();
        if (concat === "@") {
          continue;
        }
        // keywords
        chunk.appendString(c.concatTokens() + " ");
      } else if (c.get() instanceof abaplint.Expressions.SQLFieldName) {
        chunk.appendChunk(new SQLFieldNameTranspiler().transpile(c, traversal));
      } else if (c.get() instanceof abaplint.Expressions.SimpleFieldChain2) {
        chunk.appendString(`'" + ` + new FieldChainTranspiler().transpile(c, traversal).getCode() + `.get() + "' `);
      } else {
        chunk.appendString(c.concatTokens() + " ");
      }
    }

    return chunk;
  }

}