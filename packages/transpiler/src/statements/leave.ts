import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler.js";
import {Traversal} from "../traversal.js";
import {Chunk} from "../chunk.js";

export class LeaveTranspiler implements IStatementTranspiler {

  public transpile(_node: abaplint.Nodes.StatementNode, _traversal: Traversal): Chunk {
    return new Chunk(`throw new Error("Leave, transpiler todo");`);
  }

}