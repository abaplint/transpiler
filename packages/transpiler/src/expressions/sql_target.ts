import {Nodes} from "@abaplint/core";
import * as abaplint from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class SQLTargetTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const s = node.findDirectExpression(abaplint.Expressions.Target);
    return traversal.traverse(s);
  }

}