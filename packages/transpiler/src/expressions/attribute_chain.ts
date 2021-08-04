import {Expressions, Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class AttributeChainTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const ret = new Chunk();

    for (const c of node.getChildren()) {
      if (c.get() instanceof Expressions.AttributeName) {
        ret.appendChunk(traversal.traverse(c));
      } else {
        ret.append("AttributeChainTodo", node, traversal);
      }
    }

    return ret;
  }

}