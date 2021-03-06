import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";

export class GetTimeTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    const concat = node.concatTokens();
    let options = "";
    if (concat.startsWith("GET TIME FIELD")) {
      options = "{field: " + traversal.traverse(node.findFirstExpression(abaplint.Expressions.Target)) + "}";
    } else if (concat.startsWith("GET TIME STAMP FIELD")) {
      options = "{stamp: " + traversal.traverse(node.findFirstExpression(abaplint.Expressions.Target)) + "}";
    }
    return "abap.statements.getTime(" + options + ");";
  }

}