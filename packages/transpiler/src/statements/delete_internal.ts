import * as abaplint from "abaplint";
import {IStatementTranspiler} from "./_statement_transpiler";
import {TargetTranspiler, ComponentCondTranspiler} from "../expressions";

export class DeleteInternalTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode): string {
    const target = new TargetTranspiler().transpile(node.findFirstExpression(abaplint.Expressions.Target)!);

    let extra = "";
    const where = node.findFirstExpression(abaplint.Expressions.ComponentCond);
    if (where) {
      extra = ',' + new ComponentCondTranspiler().transpile(where);
    }

    return "abap.statements.deleteInternal(" + target + extra + ");";
  }

}