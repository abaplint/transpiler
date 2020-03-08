import {Nodes, Expressions} from "abaplint";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {MethodCallTranspiler, FieldChainTranspiler} from ".";

export class MethodCallChainTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode): string {
    let ret = "";

    for (const c of node.getChildren()) {
      if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.MethodCall) {
        ret = ret + new MethodCallTranspiler().transpile(c);
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.FieldChain) {
        ret = ret + new FieldChainTranspiler().transpile(c);
      } else if (c instanceof Nodes.TokenNode) {
        ret = ret + ".";
      } else {
        ret = ret + "MethodCallChainTranspilerTodo";
      }
    }

    return ret;
  }

}