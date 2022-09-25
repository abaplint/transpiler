import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class DefineTranspiler implements IStructureTranspiler {

  public transpile(_node: abaplint.Nodes.StructureNode, _traversal: Traversal): Chunk {
    // skip, macros are expanded by abaplint
    return new Chunk("");
  }

}