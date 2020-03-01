import * as abaplint from "abaplint";
import {IStatementTranspiler} from "./_statement_transpiler";

export class ClassImplementationTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode): string {
    const name = node.findFirstExpression(abaplint.Expressions.ClassName)!.getFirstToken().getStr();
    return "class " + name + " {";
  }

}