import {Nodes} from "@abaplint/core";
import {Traversal} from "../traversal";

export interface IExpressionTranspiler {
  transpile(node: Nodes.ExpressionNode, traversal: Traversal): string;
}