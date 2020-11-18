import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";

export class FunctionModuleTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): string {
    let r = "";
    for (const c of node.getChildren()) {
      if (c.get() instanceof abaplint.Statements.FunctionModule) {
        r += "function moo(input) {\n";
      } else if (c.get() instanceof abaplint.Statements.EndFunction) {
        r += "}";
      } else {
        r += traversal.traverse(c);
      }
    }
    return r;
  }

}