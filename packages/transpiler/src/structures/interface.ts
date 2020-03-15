import * as abaplint from "abaplint";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";

export class InterfaceTranspiler implements IStructureTranspiler {

  public transpile(_node: abaplint.Nodes.StructureNode, _traversal: Traversal): string {
    return "";
  }

}