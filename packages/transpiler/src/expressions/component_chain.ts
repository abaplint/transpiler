import {Expressions, Nodes} from "@abaplint/core";
import {Chunk} from "../chunk.js";
import {Traversal} from "../traversal.js";
import {IExpressionTranspiler} from "./_expression_transpiler.js";

export class ComponentChainTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const ret = new Chunk();
    for (const n of node.getChildren()) {
      if (n.get() instanceof Expressions.ComponentName) {
        ret.append(n.concatTokens().toLowerCase(), n, traversal);
      } else if (n.concatTokens() === "-") {
        ret.append(".get().", n, traversal);
      }
    }
    return ret;
  }

}