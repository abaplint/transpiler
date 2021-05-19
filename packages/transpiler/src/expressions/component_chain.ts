import {Expressions, Nodes} from "@abaplint/core";
import {Traversal} from "../traversal";
import {IExpressionTranspiler} from "./_expression_transpiler";

export class ComponentChainTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, _traversal: Traversal): string {
    let ret = "";
    for (const n of node.getChildren()) {
      if (n.get() instanceof Expressions.ComponentName) {
        ret += n.concatTokens().toLowerCase();
      } else if (n.concatTokens() === "-") {
        ret += ".get().";
      }
    }
    return ret;
  }

}