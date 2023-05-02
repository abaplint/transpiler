import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler.js";
import {Traversal} from "../traversal.js";
import {Chunk} from "../chunk.js";

export class WhenTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): Chunk {
    const ret = new Chunk();
    for (const c of node.getChildren()) {
      ret.appendChunk(traversal.traverse(c));
    }
    ret.append("break;\n", node, traversal);
    return ret;
  }

}