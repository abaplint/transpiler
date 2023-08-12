import {Nodes} from "@abaplint/core";
import * as abaplint from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {DatabaseTableTranspiler} from "./database_table";

export class SQLFromSourceTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const chunk = new Chunk();

    for (const c of node.getChildren()) {
      const concat = c.concatTokens();
      if (c instanceof abaplint.Nodes.TokenNode) {
        // keywords
        chunk.appendString(concat + " ");
      } else if (c.get() instanceof abaplint.Expressions.DatabaseTable) {
        chunk.appendString(`" + ` + new DatabaseTableTranspiler().transpile(c, traversal).getCode() + ` + "`);
        chunk.appendString(" ");
      } else {
        chunk.appendString(concat + " ");
      }
    }

    return chunk;
  }

}