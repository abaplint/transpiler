import {Expressions, Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class CondSubTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    let ret = "";

    for (const c of node.getChildren()) {
      if (c.get() instanceof Expressions.Cond) {
        ret += traversal.traverse(c).getCode();
      } else if (c instanceof Nodes.TokenNode && c.getFirstToken().getStr() === "NOT") {
        ret += "!";
      } else if (c instanceof Nodes.TokenNode && c.getFirstToken().getStr().trim() === "(") {
        ret += "(";
      } else if (c instanceof Nodes.TokenNode && c.getFirstToken().getStr().trim() === ")") {
        ret += ")";
      }
    }

    return new Chunk(ret);
  }

}