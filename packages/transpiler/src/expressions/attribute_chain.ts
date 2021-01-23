import {Expressions, Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";

export class AttributeChainTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): string {
    let ret = "";

    for (const c of node.getChildren()) {
      if (c.get() instanceof Expressions.AttributeName) {
        ret += traversal.traverse(c);
      } else {
        ret += "AttributeChainTodo";
      }
    }

    return ret;
  }

}