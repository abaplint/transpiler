import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class StopTranspiler implements IStatementTranspiler {

  /** leaves the selection events of the report, END-OF-SELECTION still runs, see ReportEvents */
  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    return new Chunk().append("throw new abap.ReportStop();", node, traversal);
  }

}
