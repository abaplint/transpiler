import {Nodes} from "@abaplint/core";
import * as abaplint from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {SQLJoinTranspiler} from "./sql_join";
import {SQLFromSourceTranspiler} from "./sql_from_source";

export class SQLFromTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const chunk = new Chunk();

    for (const c of node.getChildren()) {
      if (c instanceof abaplint.Nodes.TokenNode) {
        // keywords
        chunk.appendString(c.concatTokens() + " ");
      } else if (c.get() instanceof abaplint.Expressions.SQLJoin) {
        chunk.appendChunk(new SQLJoinTranspiler().transpile(c, traversal));
        chunk.appendString(" ");
      } else if (c.get() instanceof abaplint.Expressions.SQLFromSource) {
        chunk.appendChunk(new SQLFromSourceTranspiler().transpile(c, traversal));
      } else {
        if (c.findFirstExpression(abaplint.Expressions.Dynamic)) {
          let concat = c.concatTokens();
          if (concat.startsWith("'")) {
            // no escaping needed, guess its not possible to have table names with single or double quote part of name
            concat = "\\\"" + concat.substring(1, concat.length - 1) + "\\\"";
          }
          chunk.appendString(concat + " ");
        } else if (c.concatTokens().includes("/")) {
          chunk.appendString("\\\"" + c.concatTokens() + "\\\" ");
        } else {
          const concat = c.concatTokens();
          chunk.appendString(concat + " ");
        }
      }
    }

    return chunk;
  }

}