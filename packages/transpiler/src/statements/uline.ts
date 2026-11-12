import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class UlineTranspiler implements IStatementTranspiler {

  public transpile(_node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const chunk = new Chunk();
    chunk.appendString(traversal.beforeListOutput());
    chunk.appendString(`abap.statements.write("--------------------------------------------------", {"newLine": true});`);
    return chunk;
  }

}