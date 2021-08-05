import {Expressions, Nodes} from "@abaplint/core";
import {Chunk} from "../chunk";
import {Traversal} from "../traversal";
import {IExpressionTranspiler} from "./_expression_transpiler";

export class ComponentChainTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, _traversal: Traversal): Chunk {
    let ret = "";
    for (const n of node.getChildren()) {
      if (n.get() instanceof Expressions.ComponentName) {
        ret += n.concatTokens().toLowerCase();
      } else if (n.concatTokens() === "-") {
        ret += ".get().";
      }
    }
    return new Chunk(ret);
  }

}