import {Nodes} from "@abaplint/core";
import * as abaplint from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {SQLFieldNameTranspiler} from "./sql_field_name";

export class SQLGroupByTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const fields: string[] = [];
    for (const f of node.getChildren()) {
      if (f instanceof abaplint.Nodes.TokenNode) {
        // keywords
        continue;
      } else if (f instanceof abaplint.Nodes.ExpressionNode && f.get() instanceof abaplint.Expressions.SQLFieldName) {
        const code = new SQLFieldNameTranspiler().transpile(f, traversal).getCode();
        fields.push(code);
      } else {
        const concat = f.concatTokens();
        if (concat !== ",") {
          fields.push(concat);
        }
      }
    }

    return new Chunk().appendString("GROUP BY ").append(fields.join(", "), node, traversal);
  }

}