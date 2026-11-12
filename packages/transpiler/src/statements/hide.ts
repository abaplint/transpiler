import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class HideTranspiler implements IStatementTranspiler {

  public transpile(_node: abaplint.Nodes.StatementNode, _traversal: Traversal): Chunk {
    // HIDE keeps a field's value for the list line, for the line selection of an interactive list
    // (AT LINE-SELECTION), it does not change the list itself
    return new Chunk("");
  }

}
