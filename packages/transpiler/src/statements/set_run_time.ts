import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class SetRunTimeTranspiler implements IStatementTranspiler {

  public transpile(_node: abaplint.Nodes.StatementNode, _traversal: Traversal): Chunk {
    // SET RUN TIME ANALYZER and SET RUN TIME CLOCK RESOLUTION only affect runtime measurements
    return new Chunk(``);
  }

}
