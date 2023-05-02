import {Nodes} from "@abaplint/core";
import {Chunk} from "../chunk.js";
import {Traversal} from "../traversal.js";
import {IExpressionTranspiler} from "./_expression_transpiler.js";

export class MessageNumberTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    return new Chunk().append("'" + node.concatTokens() + "'", node, traversal);
  }

}