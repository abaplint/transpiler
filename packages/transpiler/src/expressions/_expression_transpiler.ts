import {Nodes} from "abaplint";
import {Traversal} from "../traversal";

export interface IExpressionTranspiler {
  transpile(node: Nodes.ExpressionNode, traversal: Traversal): string;
}