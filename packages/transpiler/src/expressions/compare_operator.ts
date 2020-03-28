import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";

export class CompareOperatorTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode): string {
// todo, this is not correct
    switch(node.getFirstToken().getStr().toUpperCase()) {
      case "=":
      case "EQ":
        return "eq";
      case "<":
      case "LT":
        return "lt";
      case "<=":
      case "LE":
        return "le";
      case ">":
      case "GT":
        return "gt";
      case ">=":
      case "GE":
        return "ge";
      case "<>":
      case "NE":
        return "ne";
      default:
        return "compareoperatortodo";
    }
  }

}