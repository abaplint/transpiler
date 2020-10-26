import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";

export class DeleteInternalTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    const target = traversal.traverse(node.findFirstExpression(abaplint.Expressions.Target));

    const extra: string[] = [];
    const where = node.findFirstExpression(abaplint.Expressions.ComponentCond);
    if (where) {
      extra.push("where: " + traversal.traverse(where));
    }

// todo, this is not completely correct, fields might have the name ADJACENT
    if (node.findDirectTokenByText("ADJACENT")) {
      extra.push("adjacent: true");
    }

    const index = node.findExpressionAfterToken("INDEX");
    if (index) {
      extra.push("index: " + traversal.traverse(index));
    }

    let concat = "";
    if (extra.length > 0) {
      concat = ",{" + extra.join(",") + "}";
    }

    return "abap.statements.deleteInternal(" + target + concat + ");";
  }

}