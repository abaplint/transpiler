import * as abaplint from "abaplint";
import {IStatementTranspiler} from "./_statement_transpiler";
import {CondTranspiler} from "../expressions";

export class AssertTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, spaghetti: abaplint.SpaghettiScope, filename: string): string {
    const cond = new CondTranspiler().transpile(node.findDirectExpression(abaplint.Expressions.Cond)!, spaghetti, filename);
    return "abap.statements.assert(" + cond + ");";
  }

}