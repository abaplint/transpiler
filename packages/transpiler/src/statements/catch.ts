import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";

export class CatchTranspiler implements IStatementTranspiler {

  public transpile(_node: abaplint.Nodes.StatementNode): string {
    // todo, add more here
    return "} catch (e) {";
  }

}