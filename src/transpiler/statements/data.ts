import * as abaplint from "abaplint";
import {IStatementTranspiler} from "./_statement_transpiler";

export class DataTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode): string {
    const name = node.findFirstExpression(abaplint.Expressions.NamespaceSimpleName)!.getFirstToken().getStr();
    const type = node.findFirstExpression(abaplint.Expressions.TypeName)!.getFirstToken().getStr();
    return "let " + name + " = new abap.basictypes." + type + "();";
  }

}