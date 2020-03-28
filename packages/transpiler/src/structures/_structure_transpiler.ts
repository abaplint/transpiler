import {Nodes} from "@abaplint/core";
import {Traversal} from "../traversal";

export interface IStructureTranspiler {
  transpile(node: Nodes.StructureNode, traversal: Traversal): string;
}