import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";

export class ArithOperatorTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode): string {
    switch(node.getFirstToken().getStr()) {
      case "+":
        return "abap.operators.add";
      case "-":
        return "abap.operators.minus";
      case "*":
        return "abap.operators.multiply";
      case "/":
        return "abap.operators.divide";
      case "**":
        return "abap.operators.power";
      case "DIV":
        return "abap.operators.div";
      case "MOD":
        return "abap.operators.mod";
      default:
        return ".ArithOperatorUnknown";
    }
  }

}