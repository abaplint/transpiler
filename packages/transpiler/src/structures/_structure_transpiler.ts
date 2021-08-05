import {Nodes} from "@abaplint/core";
import {Chunk} from "../chunk";
import {Traversal} from "../traversal";

export interface IStructureTranspiler {
  transpile(node: Nodes.StructureNode, traversal: Traversal): Chunk;
}