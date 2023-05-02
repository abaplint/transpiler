import {Nodes} from "@abaplint/core";
import * as abaplint from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler.js";
import {Traversal} from "../traversal.js";
import {Chunk} from "../chunk.js";

export class SQLSourceSimpleTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    let s = node.findDirectExpression(abaplint.Expressions.Source);
    if (s === undefined) {
      s = node.findDirectExpression(abaplint.Expressions.SimpleSource3);
    }
    return traversal.traverse(s);
  }

}