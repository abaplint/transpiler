import {Nodes} from "@abaplint/core";
import {Chunk} from "../chunk";
import {Traversal} from "../traversal";

export interface IExpressionTranspiler {
  transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk;
}