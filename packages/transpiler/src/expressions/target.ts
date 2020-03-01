import {Expressions, Nodes} from "abaplint";
import {IExpressionTranspiler} from "./_expression_transpiler";

export class TargetTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode): string {
    const name = node.findFirstExpression(Expressions.TargetField)!.getFirstToken().getStr();
    return name;
  }

}