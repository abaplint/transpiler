import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";

export class DescribeTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {

    const options: string[] = [];
    const concat = node.concatTokens().toUpperCase();

    const field = node.findExpressionAfterToken("FIELD");
    if (field) {
      options.push("field: " + traversal.traverse(field));
    }

    const type = node.findExpressionAfterToken("TYPE");
    if (type) {
      options.push("type: " + traversal.traverse(type));
    }

    const length = node.findExpressionAfterToken("LENGTH");
    if (length) {
      options.push("length: " + traversal.traverse(length));
    }

    if (concat.includes("IN CHARACTER MODE")) {
      options.push("mode: 'CHARACTER'");
    }

    if (concat.includes("IN BYTE MODE")) {
      options.push("mode: 'BYTE'");
    }

    return "abap.statements.describe({" + options.join(", ") + "});";
  }

}