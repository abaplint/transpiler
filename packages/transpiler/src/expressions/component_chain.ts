import {Expressions, Nodes} from "@abaplint/core";
import {Chunk} from "../chunk";
import {Traversal} from "../traversal";
import {IExpressionTranspiler} from "./_expression_transpiler";

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

  public static concat(node: Nodes.ExpressionNode, _traversal: Traversal): string {
    let ret = "";
    for (const n of node.getChildren()) {
      if (n.get() instanceof Expressions.ComponentName) {
        ret += n.concatTokens().toLowerCase();
      } else {
        ret += n.concatTokens();
      }
    }
    return ret;
  }

}