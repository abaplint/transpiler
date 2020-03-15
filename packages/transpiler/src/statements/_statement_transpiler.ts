import {Nodes} from "abaplint";
import {Traversal} from "../traversal";

export interface IStatementTranspiler {
  transpile(node: Nodes.StatementNode, traversal: Traversal): string;
}