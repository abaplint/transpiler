import {Nodes, Expressions} from "abaplint";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {MethodCallTranspiler, FieldChainTranspiler} from ".";
import {Traversal} from "../traversal";

export class MethodCallChainTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): string {
    let ret = "";

    for (const c of node.getChildren()) {
      if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.MethodCall) {
        ret = ret + new MethodCallTranspiler().transpile(c, traversal);
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.FieldChain) {
        ret = ret + new FieldChainTranspiler().transpile(c, traversal);
      } else if (c instanceof Nodes.TokenNode) {
        ret = ret + ".get().";
      } else {
        ret = ret + "MethodCallChainTranspilerTodo";
      }
    }

    if (node.getFirstChild()?.get() instanceof Expressions.MethodCall
        && !ret.startsWith("abap.builtin.")) {
      ret = "this." + ret;
    }

    return ret;
  }

}