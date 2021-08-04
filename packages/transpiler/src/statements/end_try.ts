import * as abaplint from "@abaplint/core";
import {Chunk} from "../chunk";
import {IStatementTranspiler} from "./_statement_transpiler";

export class EndTryTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode): Chunk {
    return new Chunk("}", node.getFirstToken().getStart());
  }

}