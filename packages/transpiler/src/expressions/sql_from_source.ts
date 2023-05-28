import {Nodes} from "@abaplint/core";
import * as abaplint from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class SQLFromSourceTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, _traversal: Traversal): Chunk {
    const chunk = new Chunk();

    for (const c of node.getChildren()) {
      const concat = c.concatTokens();
      if (c instanceof abaplint.Nodes.TokenNode) {
        // keywords
        chunk.appendString(concat + " ");
      } else if (c.get() instanceof abaplint.Expressions.DatabaseTable) {
        let val = `" + abap.dbo.schemaPrefix + "\\"" + abap.dbo.tablePrefix + "`;
        if(concat.startsWith("('")) {
          val += concat.substring(2, concat.length - 2).toLowerCase();
        } else {
          val += concat;
        }
        val += "\\\" ";
        chunk.appendString(val);
      } else {
        chunk.appendString(concat + " ");
      }
    }

    return chunk;
  }

}