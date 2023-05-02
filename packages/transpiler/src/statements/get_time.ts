import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler.js";
import {Traversal} from "../traversal.js";
import {Chunk} from "../chunk.js";

export class GetTimeTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const concat = node.concatTokens().toUpperCase();
    let options = "";
    if (concat.startsWith("GET TIME FIELD")) {
      options = "{field: " + traversal.traverse(node.findFirstExpression(abaplint.Expressions.Target)).getCode() + "}";
    } else if (concat.startsWith("GET TIME STAMP FIELD")) {
      options = "{stamp: " + traversal.traverse(node.findFirstExpression(abaplint.Expressions.Target)).getCode() + "}";
    }
    return new Chunk()
      .append("abap.statements.getTime(", node, traversal)
      .appendString(options)
      .append(");", node.getLastToken(), traversal);
  }

}