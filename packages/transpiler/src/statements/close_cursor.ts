import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class CloseCursorTranspiler implements IStatementTranspiler {

  public transpile(_node: abaplint.Nodes.StatementNode, _traversal: Traversal): Chunk {
    const ret = new Chunk();
    ret.appendString("closecursortodo");
    return ret;
  }

}