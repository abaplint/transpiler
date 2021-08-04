import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class DescribeTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {

    const options: string[] = [];
    const concat = node.concatTokens().toUpperCase();

    const field = node.findExpressionAfterToken("FIELD");
    if (field) {
      options.push("field: " + traversal.traverse(field).getCode());
    }

    const type = node.findExpressionAfterToken("TYPE");
    if (type) {
      options.push("type: " + traversal.traverse(type).getCode());
    }

    const length = node.findExpressionAfterToken("LENGTH");
    if (length) {
      options.push("length: " + traversal.traverse(length).getCode());
    }

    if (concat.includes("IN CHARACTER MODE")) {
      options.push("mode: 'CHARACTER'");
    }

    if (concat.includes("IN BYTE MODE")) {
      options.push("mode: 'BYTE'");
    }

    return new Chunk("abap.statements.describe({" + options.join(", ") + "});");
  }

}