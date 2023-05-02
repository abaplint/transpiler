import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler.js";
import {Traversal} from "../traversal.js";
import {Chunk} from "../chunk.js";


export class AtLastTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): Chunk {
    const ret = new Chunk();

    const body = node.findDirectStructure(abaplint.Structures.Body);
    if (body) {
      ret.appendChunk(traversal.traverse(body));
    }

    return ret;
  }

}