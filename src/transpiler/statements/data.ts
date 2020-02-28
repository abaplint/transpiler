import * as abaplint from "abaplint";
import {IStatementTranspiler} from "./_statement_transpiler";

export class DataTranspiler extends abaplint.Statements.Data implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode): string {
    const name = node.findFirstExpression(abaplint.Expressions.NamespaceSimpleName)!.getFirstToken().getStr();
    const type = node.findFirstExpression(abaplint.Expressions.TypeName)!.getFirstToken().getStr();

    let value = "";
    const val = node.findFirstExpression(abaplint.Expressions.Value);
    if (val) {
      const int = val.findFirstExpression(abaplint.Expressions.Integer);
      if (int){
        value = int.getFirstToken().getStr();
      }
    }

    return "let " + name + " = new abap.basictypes." + type + "(" + value + ");";
  }

}