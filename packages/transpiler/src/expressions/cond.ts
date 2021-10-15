import {Expressions, Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class CondTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const ret = new Chunk();

    for (const c of node.getChildren()) {
      if (c.get() instanceof Expressions.Compare
          || c.get() instanceof Expressions.CondSub) {
        ret.appendChunk(traversal.traverse(c));
      } else if (c instanceof Nodes.TokenNode
          && c.getFirstToken().getStr().toUpperCase() === "OR") {
        ret.append(" || ", c, traversal);
      } else if (c instanceof Nodes.TokenNode
          && c.getFirstToken().getStr().toUpperCase() === "AND") {
        ret.append(" && ", c, traversal);
      }
    }

    return ret;
  }

}