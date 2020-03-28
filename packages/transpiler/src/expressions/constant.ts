import {Expressions, Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";

export class ConstantTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode): string {
    const int = node.findFirstExpression(Expressions.Integer);
    if (int) {
      return int.getFirstToken().getStr();
    }

    const str = node.findFirstExpression(Expressions.ConstantString);
    if (str) {
      return str.getFirstToken().getStr();
    }

    return "todo, Constant";
  }

}