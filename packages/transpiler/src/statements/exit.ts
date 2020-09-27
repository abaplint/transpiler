import * as abaplint from "@abaplint/core";
import {Traversal} from "../traversal";
import {IStatementTranspiler} from "./_statement_transpiler";

export class ExitTranspiler implements IStatementTranspiler {

  public transpile(_node: abaplint.Nodes.StatementNode, _traversal: Traversal): string {
    return "break;";
  }

}