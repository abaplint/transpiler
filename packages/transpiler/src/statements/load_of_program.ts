import * as abaplint from "@abaplint/core";
import {Chunk} from "../chunk";
import {IStatementTranspiler} from "./_statement_transpiler";

export class LoadOfProgramTranspiler implements IStatementTranspiler {

  public transpile(_node: abaplint.Nodes.StatementNode): Chunk {
    return new Chunk(`throw new Error("LOAD-OF-PROGRAM not supported, transpiler");`);
  }

}
