import {Nodes} from "@abaplint/core";
import {Chunk} from "../chunk.js";
import {Traversal} from "../traversal.js";

export interface IStructureTranspiler {
  transpile(node: Nodes.StructureNode, traversal: Traversal): Chunk;
}