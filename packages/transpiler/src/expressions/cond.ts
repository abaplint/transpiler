import {Expressions, Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class CondTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    let ret = "";

    for (const c of node.getChildren()) {
      if (c.get() instanceof Expressions.Compare
          || c.get() instanceof Expressions.CondSub) {
        ret += traversal.traverse(c).getCode();
      } else if (c instanceof Nodes.TokenNode && c.getFirstToken().getStr() === "OR") {
        ret += " || ";
      } else if (c instanceof Nodes.TokenNode && c.getFirstToken().getStr() === "AND") {
        ret += " && ";
      }
    }

    return new Chunk(ret);
  }

}