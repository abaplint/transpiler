import {Nodes, Expressions} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";

export class MethodCallChainTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): string {
    let ret = "";

    for (const c of node.getChildren()) {
      if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.MethodCall) {
        const sub = traversal.traverse(c);
        if (sub.startsWith("abap.builtin.")) {
          ret = ret + sub;
        } else {
          const t = c === node.getFirstChild() ? "this." : "";
          ret = "(await " + t + ret + sub + ")";
        }
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.FieldChain) {
        ret = ret + traversal.traverse(c);
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.ClassName) {
        ret = traversal.lookupClass(c.getFirstToken().getStr(), c.getFirstToken());
      } else if (c instanceof Nodes.ExpressionNode && c.get() instanceof Expressions.MethodName) {
        ret += c.getFirstToken().getStr().toLowerCase();
      } else if (c instanceof Nodes.TokenNode && c.getFirstToken().getStr() === "->") {
        if (ret === "super") {
          ret = ret + ".";
        } else {
          ret = ret + ".get().";
        }
      } else if (c instanceof Nodes.TokenNode && c.getFirstToken().getStr() === "=>") {
        ret = ret + ".";
      } else {
        ret = ret + "MethodCallChainTranspilerTodo-" + c.get().constructor.name;
      }
    }

    if (ret.startsWith("(") && ret.endsWith(")")) {
      return ret.substr(1, ret.length - 2);
    } else {
      return ret;
    }
  }

}