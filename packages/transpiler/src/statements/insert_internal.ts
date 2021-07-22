import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";

export class InsertInternalTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    const options: string[] = [];

    let source = node.findDirectExpression(abaplint.Expressions.SimpleSource1);
    const target = node.findDirectExpression(abaplint.Expressions.Target);

    const concat = node.concatTokens().toUpperCase();
    if (concat.startsWith("INSERT LINES OF ")) {
      source = node.findDirectExpression(abaplint.Expressions.Source);
      options.push("lines: true");
    }

    if (source === undefined || target === undefined) {
      throw "InsertInternalTranspiler, source or target not found: " + node.concatTokens();
    }

    const index = node.findExpressionAfterToken("INDEX");
    if (index) {
      options.push("index: " + traversal.traverse(index));
    }

    const assigning = node.findExpressionAfterToken("ASSIGNING");
    if (assigning) {
      options.push("assigning: " + traversal.traverse((assigning.findFirstExpression(abaplint.Expressions.FieldSymbol))));
    }

    const s = traversal.traverse(source);
    const t = traversal.traverse(target);

    return `abap.statements.insertInternal(${s}, ${t}, {${options.join(", ")}});`;
  }

}