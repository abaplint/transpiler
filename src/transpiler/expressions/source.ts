import {Expressions, Nodes} from "abaplint";
import {IExpressionTranspiler} from "./_expression_transpiler";

export class SourceTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode): string {
    const int = node.findFirstExpression(Expressions.Integer);
    if (int) {
      return int.getFirstToken().getStr();
    }

    const name = node.findFirstExpression(Expressions.SourceField);
    if (name) {
      return name.getFirstToken().getStr();
    }

    return "todo, SourceTranspiler";
  }

}