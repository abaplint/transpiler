import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class SetRunTimeTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    if (node.findDirectTokenByText("CLOCK")) {
      const resolution = node.findDirectTokenByText("LOW") ? "LOW" : "HIGH";
      return new Chunk().append(`abap.statements.setRunTimeClock("${resolution}");`, node, traversal);
    }
    // SET RUN TIME ANALYZER ON | OFF only switches the runtime analysis
    return new Chunk(``);
  }

}
