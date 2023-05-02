import * as abaplint from "@abaplint/core";
import {Chunk} from "../chunk.js";
import {IStatementTranspiler} from "./_statement_transpiler.js";

export class TypeTranspiler implements IStatementTranspiler {

  public transpile(_node: abaplint.Nodes.StatementNode): Chunk {
    return new Chunk("");
  }

}