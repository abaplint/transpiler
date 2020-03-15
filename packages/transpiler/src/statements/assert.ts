import * as abaplint from "abaplint";
import {IStatementTranspiler} from "./_statement_transpiler";
import {CondTranspiler} from "../expressions";
import {Traversal} from "../traversal";

export class AssertTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    const cond = new CondTranspiler().transpile(node.findDirectExpression(abaplint.Expressions.Cond)!, traversal);
    return "abap.statements.assert(" + cond + ");";
  }

}