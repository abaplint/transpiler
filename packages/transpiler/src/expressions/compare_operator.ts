import {Nodes} from "abaplint";
import {IExpressionTranspiler} from "./_expression_transpiler";

export class CompareOperatorTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode): string {
// todo, this is not correct
    switch(node.getFirstToken().getStr().toUpperCase()) {
      case '=':
      case 'EQ':
        return "eq";
      case '<>':
      case 'NE':
        return "ne";
      default:
        return "compareoperatortodo";
    }
  }

}