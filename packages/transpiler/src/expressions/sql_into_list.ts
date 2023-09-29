import {Nodes} from "@abaplint/core";
import * as abaplint from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class SQLIntoListTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const targets = node.findDirectExpressions(abaplint.Expressions.SQLTarget);
    const transpiled = targets.map(t=> traversal.traverse(t));
    const chunk = new Chunk();
    chunk.appendString("[");
    chunk.appendString(transpiled.map(t => t.getCode()).join(","));
    chunk.appendString("]");
    return chunk;
  }

}