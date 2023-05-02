import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler.js";
import {Traversal} from "../traversal.js";
import {Chunk} from "../chunk.js";

export class TypesTranspiler implements IStructureTranspiler {

  public transpile(_node: abaplint.Nodes.StructureNode, _traversal: Traversal): Chunk {
    return new Chunk("");
  }

}