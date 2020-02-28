import {Expressions, Nodes} from "abaplint";
import {IExpressionTranspiler} from "./_expression_transpiler";

export class SourceTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode): string {
    const int = node.findFirstExpression(Expressions.Integer)!.getFirstToken().getStr();
    return int;
  }

}