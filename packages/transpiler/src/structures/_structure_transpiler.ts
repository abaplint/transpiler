import {Nodes} from "abaplint";
import {Traversal} from "../traversal";

export interface IStructureTranspiler {
  transpile(node: Nodes.StructureNode, traversal: Traversal): string;
}