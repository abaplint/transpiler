import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {InlineDeclarations} from "../inline";

export class MethodTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): Chunk {
    const ret = new Chunk();

    for (const c of node.getChildren()) {
      ret.appendChunk(traversal.traverse(c));

      if (c.get() instanceof abaplint.Statements.MethodImplementation) {
        const declare = InlineDeclarations.buildDeclarations(node, traversal);
        ret.appendString(declare);
      }
    }

    return ret;
  }

}