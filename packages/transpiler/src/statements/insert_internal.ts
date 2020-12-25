import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";

export class InsertInternalTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    const options: string[] = [];

    const source = node.findDirectExpression(abaplint.Expressions.SimpleSource);
    const target = node.findDirectExpression(abaplint.Expressions.Target);
    if (source === undefined || target === undefined) {
      throw "InsertInternalTranspiler, source or target not found";
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