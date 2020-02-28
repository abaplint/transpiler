import {Expressions, Nodes} from "abaplint";
import {IExpressionTranspiler} from "./_expression_transpiler";

export class ConstantTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode): string {
    const int = node.findFirstExpression(Expressions.Integer);
    if (int) {
      return int.getFirstToken().getStr();
    }

    return "todo, Constant";
  }

}