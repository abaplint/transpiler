import * as abaplint from "@abaplint/core";
import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {SQLFieldNameTranspiler} from "./sql_field_name";

export class SQLFieldAndValueTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const chunk = new Chunk();

    chunk.appendString("\"");
    for (const n of node.getChildren()) {
      const concat = n.concatTokens();
      if (concat === "=") {
        chunk.appendString(" = ");
      } else if (n.get() instanceof abaplint.Expressions.SQLFieldName) {
        chunk.appendString(new SQLFieldNameTranspiler().transpile(n as Nodes.ExpressionNode, traversal).getCode());
      } else if (n.get() instanceof abaplint.Expressions.SQLSource) {
        const source = traversal.traverse(n);
        // todo: value conversion? abap vs db, traversal.isSQLConversion
        // todo: integers?
        chunk.appendString("'\" + " + source.getCode() + ".get() + \"'");
      } else if (n instanceof abaplint.Nodes.TokenNode) {
        // operators/keywords, eg. the "+" in "field = field + value"
        chunk.appendString(" " + concat + " ");
      } else {
        chunk.appendString(traversal.traverse(n).getCode() + " ");
      }
    }
    chunk.appendString("\"");

    return chunk;
  }

}