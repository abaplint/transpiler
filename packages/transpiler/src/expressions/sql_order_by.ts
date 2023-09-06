import {Nodes} from "@abaplint/core";
import * as abaplint from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {SQLFieldNameTranspiler} from "./sql_field_name";

export class SQLOrderByTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const ret = new Chunk();
    for (const c of node.getChildren()) {
      if (c instanceof abaplint.Nodes.ExpressionNode && c.get() instanceof abaplint.Expressions.SQLFieldName) {
        ret.appendString(new SQLFieldNameTranspiler().transpile(c, traversal).getCode() + " ");
      } else {
        ret.appendString(c.concatTokens() + " ");
      }
    }
    return ret;
  }

}