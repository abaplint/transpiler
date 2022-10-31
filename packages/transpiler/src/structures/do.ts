import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {DoTranspiler as DoStatementTranspiler, EndDoTranspiler} from "../statements";
import {UniqueIdentifier} from "../unique_identifier";

export class DoTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): Chunk {
    const ret = new Chunk();
    const syIndexBackup = UniqueIdentifier.get();
    for (const c of node.getChildren()) {
      if (c instanceof abaplint.Nodes.StatementNode && c.get() instanceof abaplint.Statements.Do) {
        ret.appendChunk(new DoStatementTranspiler(syIndexBackup).transpile(c, traversal));
        ret.appendString("\n");
      } else if (c instanceof abaplint.Nodes.StatementNode && c.get() instanceof abaplint.Statements.EndDo) {
        ret.appendChunk(new EndDoTranspiler(syIndexBackup).transpile(c, traversal));
      } else {
        ret.appendChunk(traversal.traverse(c));
      }
    }
    return ret;
  }

}