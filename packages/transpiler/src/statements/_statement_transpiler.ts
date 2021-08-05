import {Nodes} from "@abaplint/core";
import {Chunk} from "../chunk";
import {Traversal} from "../traversal";

export interface IStatementTranspiler {
  transpile(node: Nodes.StatementNode, traversal: Traversal): Chunk;
}