import {Nodes} from "abaplint";
import {IExpressionTranspiler} from "./_expression_transpiler";

export class TypeNameTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode): string {

    switch (node?.getFirstToken().getStr().toUpperCase()) {
      case "I":
        return "abap.types.Integer";
      case "C":
        return "abap.types.Character";
      case "P":
        return "abap.types.Packed";
      case "STRING":
        return "abap.types.String";
      default:
        return "todo, TypeNameTranspiler";
    }

  }

}