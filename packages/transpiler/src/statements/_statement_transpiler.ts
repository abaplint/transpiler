import {Nodes} from "@abaplint/core";
import {Traversal} from "../traversal";

export interface IStatementTranspiler {
  transpile(node: Nodes.StatementNode, traversal: Traversal): string;
}