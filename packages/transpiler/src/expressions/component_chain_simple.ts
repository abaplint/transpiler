import {Nodes, Expressions} from "@abaplint/core";
import {Chunk} from "../chunk";
import {Traversal} from "../traversal";
import {IExpressionTranspiler} from "./_expression_transpiler";

export class ComponentChainSimpleTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, _traversal: Traversal): Chunk {
    let ret = "";
    for (const c of node.getChildren()) {
      if (c.get() instanceof Expressions.ComponentName) {
        ret += c.getFirstToken().getStr();
      } else if (c.get() instanceof Expressions.ArrowOrDash) {
        ret += ".get().";
      }
    }
    return new Chunk(ret);
  }

}