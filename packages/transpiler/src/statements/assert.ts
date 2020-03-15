import * as abaplint from "abaplint";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";

export class AssertTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    const cond = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Cond));
    return "abap.statements.assert(" + cond + ");";
  }

}