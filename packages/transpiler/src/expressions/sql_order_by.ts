import {Nodes} from "@abaplint/core";
import * as abaplint from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {SQLFieldNameTranspiler} from "./sql_field_name";

export class SQLOrderByTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    let text = "";
    for (const c of node.getChildren()) {
      if (c instanceof abaplint.Nodes.ExpressionNode && c.get() instanceof abaplint.Expressions.SQLFieldName) {
        if (text !== "" && text.endsWith(`" `)) {
          text += ", ";
        }
        text += new SQLFieldNameTranspiler().transpile(c, traversal).getCode().toLowerCase() + " ";
      } else {
        text += c.concatTokens() + " ";
      }
    }

    return new Chunk().appendString(text);
  }

}