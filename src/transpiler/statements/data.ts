import * as abaplint from "abaplint";
import {IStatementTranspiler} from "./_statement_transpiler";
import {TypeTranspiler} from "../expressions";

export class DataTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode): string {
    const name = node.findFirstExpression(abaplint.Expressions.NamespaceSimpleName)!.getFirstToken().getStr();

    let type = "todo, type";
    const typeExpression = node.findFirstExpression(abaplint.Expressions.Type);
    if (typeExpression) {
      type = new TypeTranspiler().transpile(typeExpression);
    }

    let value = "";
    const val = node.findFirstExpression(abaplint.Expressions.Value);
    if (val) {
      const int = val.findFirstExpression(abaplint.Expressions.Integer);
      if (int){
        value = "{value: " + int.getFirstToken().getStr() + "}";
      }
    }

    return "let " + name + " = new abap.types." + type + "(" + value + ");";
  }

}