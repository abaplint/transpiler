import * as abaplint from "abaplint";
import {IStatementTranspiler} from "./_statement_transpiler";
import {CondTranspiler} from "../expressions";

export class AssertTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode): string {
    const cond = new CondTranspiler().transpile(node.findDirectExpression(abaplint.Expressions.Cond)!);
    return "abap.statements.assert(" + cond + ");";
  }

}