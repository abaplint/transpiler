import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class WhenTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): Chunk {
    let ret = "";
    for (const c of node.getChildren()) {
      ret = ret + traversal.traverse(c).getCode();
    }
    ret = ret + "break;\n";
    return new Chunk(ret);
  }

}