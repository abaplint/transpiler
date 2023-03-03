import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";


export class AtFirstTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): Chunk {
    const ret = new Chunk();

    const body = node.findDirectStructure(abaplint.Structures.Body);
    if (body) {
      ret.appendChunk(traversal.traverse(body));
    }

    return ret;
  }

}