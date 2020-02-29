import {Nodes, Expressions} from "abaplint";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {MethodCallTranspiler} from ".";

export class MethodCallChainTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode): string {

    const call = node.findDirectExpression(Expressions.MethodCall);
    if (call) {
      return new MethodCallTranspiler().transpile(call);
    }

    return "todo, MethodCallChain";
  }

}