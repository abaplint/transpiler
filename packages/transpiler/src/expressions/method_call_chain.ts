import {Nodes, Expressions} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";

export class MethodCallChainTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): string {
    let ret = "";

    for (const c of node.getChildren()) {
      if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.MethodCall) {
        ret = ret + traversal.traverse(c);
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.FieldChain) {
        ret = ret + traversal.traverse(c);
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.ClassName) {
        ret = c.getFirstToken().getStr();
      } else if (c instanceof Nodes.TokenNode && c.getFirstToken().getStr() === "->") {
        ret = ret + ".get().";
      } else if (c instanceof Nodes.TokenNode && c.getFirstToken().getStr() === "=>") {
        ret = ret + ".";
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