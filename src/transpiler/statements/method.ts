import * as abaplint from "abaplint";
import {IStatementTranspiler} from "./_statement_transpiler";

export class MethodTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode): string {
    const name = node.findFirstExpression(abaplint.Expressions.MethodName)!.getFirstToken().getStr();
    return name + "() {";
  }

}