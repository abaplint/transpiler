import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";

export class SplitTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {

    const sources = node.findDirectExpressions(abaplint.Expressions.Source);

    const source = traversal.traverse(sources[0]);
    const at = traversal.traverse(sources[1]);

    let to = "";
    const table = node.findExpressionAfterToken("TABLE");
    if (table) {
      const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target));
      to = ", table: " + target;
    } else {
      to = ", targets: [" + node.findDirectExpressions(abaplint.Expressions.Target).map(e => traversal.traverse((e))).join(",") + "]";
    }

    return "abap.statements.split({source: " + source + ", at: " + at + to + "});";
  }

}