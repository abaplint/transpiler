import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {WhileTranspiler as WhileStatementTranspiler, EndWhileTranspiler} from "../statements";
import {UniqueIdentifier} from "../unique_identifier";

export class WhileTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): Chunk {
    const ret = new Chunk();
    const syIndexBackup = UniqueIdentifier.getIndexBackup();
    for (const c of node.getChildren()) {
      if (c instanceof abaplint.Nodes.StatementNode && c.get() instanceof abaplint.Statements.While) {
        ret.appendChunk(new WhileStatementTranspiler(syIndexBackup).transpile(c, traversal));
        ret.appendString("\n");
      } else if (c instanceof abaplint.Nodes.StatementNode && c.get() instanceof abaplint.Statements.EndWhile) {
        ret.appendChunk(new EndWhileTranspiler(syIndexBackup).transpile(c, traversal));
      } else {
        ret.appendChunk(traversal.traverse(c));
      }
    }
    return ret;
  }

}