import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";

export class ConcatenateTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    const concat = node.concatTokens();

    const slist = [];
    for (const s of node.findDirectExpressions(abaplint.Expressions.Source)) {
      slist.push(traversal.traverse(s));
    }

    let extra = "";
    if (node.findExpressionAfterToken("BY")) {
      extra = `, separatedBy: ${slist.pop()}`;
    }

    if (concat.startsWith("CONCATENATE LINES OF ")) {
      extra += ", lines: true";
    }

    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target));

    return "abap.statements.concatenate({source: [" + slist.join(",") + "], target: " + target + extra + "});";
  }

}