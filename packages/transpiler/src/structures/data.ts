import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";
import {DataTranspiler as DataStatementTranspiler} from "../statements";
import {Chunk} from "../chunk";

export class DataTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): Chunk {
    const begin = node.findDirectStatement(abaplint.Statements.DataBegin);
    if (begin === undefined) {
      return new Chunk("");
    }

    const chunk = new DataStatementTranspiler().transpile(begin, traversal);
    chunk.appendString("\n");
    return chunk;
  }

}