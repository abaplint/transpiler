import * as abaplint from "abaplint";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";

export class ClassImplementationTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): string {
    let ret = "";
    for (const c of node.getChildren()) {
      ret = ret + traversal.traverse(c);
    }
    return ret;
  }

}