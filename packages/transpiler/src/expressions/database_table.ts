import {Nodes, Expressions} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class DatabaseTableTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const ret = new Chunk();

    const dyn = node.findDirectExpression(Expressions.Dynamic);
    if (dyn) {
      const sub = dyn.getChildren()[1];
      return traversal.traverse(sub);
    } else {
      ret.appendString('"' + node.concatTokens() + '"');
    }
    return ret;
  }

}