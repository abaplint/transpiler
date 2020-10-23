import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";

export class CompareOperatorTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode): string {
    const op = node.getFirstToken().getStr().toUpperCase();
    switch(op) {
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
      case "CP":
        return "cp";
      default:
        return "compareoperatortodo" + op;
    }
  }

}