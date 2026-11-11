import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class TypeMeshTranspiler implements IStructureTranspiler {

  public transpile(_node: abaplint.Nodes.StructureNode, _traversal: Traversal): Chunk {
    // a mesh is a type, and types have no code, see TypesTranspiler
    return new Chunk("");
  }

}
