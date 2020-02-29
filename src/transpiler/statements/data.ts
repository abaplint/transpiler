import * as abaplint from "abaplint";
import {IStatementTranspiler} from "./_statement_transpiler";
import {TypeTranspiler, TypeTableTranspiler} from "../expressions";

export class DataTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode): string {
    const name = node.findFirstExpression(abaplint.Expressions.NamespaceSimpleName)!.getFirstToken().getStr();

    let type = "todo, type";
    const typeExpression = node.findFirstExpression(abaplint.Expressions.Type);
    if (typeExpression) {
      type = new TypeTranspiler().transpile(typeExpression);
    } else {
      const typeTable = node.findFirstExpression(abaplint.Expressions.TypeTable);
      if (typeTable) {
        type = new TypeTableTranspiler().transpile(typeTable);
      }
    }

    let value = "";
    const val = node.findFirstExpression(abaplint.Expressions.Value);
    if (val) {
      const int = val.findFirstExpression(abaplint.Expressions.Integer);
      if (int){
        value = "{value: " + int.getFirstToken().getStr() + "}";
      }
    }

    return "let " + name + " = new " + type + "(" + value + ");";
  }

}