import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";

export class ArithOperatorTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode): string {
    switch(node.getFirstToken().getStr()) {
      case "+":
        return ".add(";
      case "-":
        return ".minus(";
      case "*":
        return ".multiply(";
      case "/":
        return ".divide(";
      case "**":
        return ".power(";
      default:
        return ".ArithOperatorUnknown";
    }
  }

}