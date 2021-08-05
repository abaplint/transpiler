import {Nodes, Expressions} from "@abaplint/core";
import {Chunk} from "../chunk";
import {Traversal} from "../traversal";
import {IExpressionTranspiler} from "./_expression_transpiler";

export class ComponentChainSimpleTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const ret = new Chunk();
    for (const c of node.getChildren()) {
      const type = c.get();
      if (type instanceof Expressions.ComponentName) {
        ret.append(c.getFirstToken().getStr().toLowerCase(), c, traversal);
      } else if (type instanceof Expressions.ArrowOrDash) {
        ret.append(".get().", c, traversal);
      }
    }
    return ret;
  }

}