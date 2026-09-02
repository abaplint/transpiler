import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class CatchSystemExceptionsTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): Chunk {
    return new Chunk().append(`throw new Error("CATCH SYSTEM-EXCEPTIONS, not supported, transpiler");`, node, traversal);
  }

}
