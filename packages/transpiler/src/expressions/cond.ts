import {Expressions, Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";

export class CondTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): string {
    let ret = "";

    for (const c of node.getChildren()) {
      if (c.get() instanceof Expressions.Compare
          || c.get() instanceof Expressions.CondSub) {
        ret += traversal.traverse(c);
      } else if (c instanceof Nodes.TokenNode && c.getFirstToken().getStr() === "OR") {
        ret += " || ";
      } else if (c instanceof Nodes.TokenNode && c.getFirstToken().getStr() === "AND") {
        ret += " && ";
      }
    }

    return ret;
  }

}