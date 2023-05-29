import * as abaplint from "@abaplint/core";
import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class SQLFieldAndValueTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const chunk = new Chunk();
/*
    for (const n of node.getChildren()) {
      const concat =
    }
*/
    const name = traversal.traverse(node.findDirectExpression(abaplint.Expressions.SQLFieldName)).getCode();
    const source = traversal.traverse(node.findDirectExpression(abaplint.Expressions.SQLSource));
    chunk.appendString("\"\\\"" + name + "\\\" = '\" + " + source.getCode() + ".get() + \"'\"");

    return chunk;
  }

}