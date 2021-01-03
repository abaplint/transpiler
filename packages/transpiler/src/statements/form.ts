import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";

export class FormTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode): string {
    const name = node.findFirstExpression(abaplint.Expressions.FormName)!.getFirstToken().getStr();
    return "async function " + name + "() {";
  }

}