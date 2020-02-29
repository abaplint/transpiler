import * as abaplint from "abaplint";
import {IStatementTranspiler} from "./_statement_transpiler";

export class FormTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode): string {
    const name = node.findFirstExpression(abaplint.Expressions.FormName)!.getFirstToken().getStr();
    return "function " + name + "() {";
  }

}