import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler.js";
import {Traversal} from "../traversal.js";
import {Chunk} from "../chunk.js";

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

    const decimals = node.findExpressionAfterToken("DECIMALS");
    if (decimals) {
      options.push("decimals: " + traversal.traverse(decimals).getCode());
    }

    const lines = node.findExpressionAfterToken("LINES");
    if (lines) {
      options.push("lines: " + traversal.traverse(lines).getCode());
    }

    const table = node.findExpressionAfterToken("TABLE");
    if (table) {
      options.push("table: " + traversal.traverse(table).getCode());
    }

    if (concat.includes("IN CHARACTER MODE")) {
      options.push("mode: 'CHARACTER'");
    }

    if (concat.includes("IN BYTE MODE")) {
      options.push("mode: 'BYTE'");
    }

    return new Chunk()
      .append("abap.statements.describe({", node, traversal)
      .appendString(options.join(", "))
      .append("});", node.getLastToken(), traversal);
  }

}