import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";

export class EndFormTranspiler implements IStatementTranspiler {

  public transpile(_node: abaplint.Nodes.StatementNode): string {
    return "}";
  }

}