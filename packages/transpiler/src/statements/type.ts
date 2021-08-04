import * as abaplint from "@abaplint/core";
import {Chunk} from "../chunk";
import {IStatementTranspiler} from "./_statement_transpiler";

export class TypeTranspiler implements IStatementTranspiler {

  public transpile(_node: abaplint.Nodes.StatementNode): Chunk {
    return new Chunk("");
  }

}