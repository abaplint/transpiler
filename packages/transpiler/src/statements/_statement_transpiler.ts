import {Nodes} from "@abaplint/core";
import {Chunk} from "../chunk.js";
import {Traversal} from "../traversal.js";

export interface IStatementTranspiler {
  transpile(node: Nodes.StatementNode, traversal: Traversal): Chunk;
}