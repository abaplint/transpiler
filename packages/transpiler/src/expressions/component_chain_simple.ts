import {Nodes, Expressions} from "@abaplint/core";
import {Traversal} from "../traversal";
import {IExpressionTranspiler} from "./_expression_transpiler";

export class ComponentChainSimpleTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, _traversal: Traversal): string {
    let ret = "";
    for (const c of node.getChildren()) {
      if (c.get() instanceof Expressions.ComponentName) {
        ret += c.getFirstToken().getStr();
      } else if (c.get() instanceof Expressions.ArrowOrDash) {
        ret += ".get().";
      }
    }
    return ret;
  }

}