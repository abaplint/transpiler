import {Nodes} from "@abaplint/core";
import {Chunk} from "../chunk.js";
import {Traversal} from "../traversal.js";

export interface IExpressionTranspiler {
  transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk;
}