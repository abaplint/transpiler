import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";

export class ClearTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target));
    return "abap.statements.clear(" + target + ");";
  }

}