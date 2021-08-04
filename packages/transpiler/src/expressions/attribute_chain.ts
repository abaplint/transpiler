import {Expressions, Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class AttributeChainTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    let ret = "";

    for (const c of node.getChildren()) {
      if (c.get() instanceof Expressions.AttributeName) {
        ret += traversal.traverse(c).getCode();
      } else {
        ret += "AttributeChainTodo";
      }
    }

    return new Chunk(ret);
  }

}