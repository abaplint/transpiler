import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";

export class SortTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    let extra = "";
    const descending = node.findDirectTokenByText("DESCENDING") !== undefined;
    if (descending === true) {
      extra = ", {descending: true}";
    }
    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target));
    return "abap.statements.sort(" + target + extra + ");";
  }

}