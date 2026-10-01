import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class LeaveTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, _traversal: Traversal): Chunk {
    if (node.concatTokens().toUpperCase() === "LEAVE PROGRAM.") {
      // ends the program; a SUBMIT ... AND RETURN that started it continues
      return new Chunk(`throw new abap.LeaveProgram();`);
    }
    return new Chunk(`throw new Error("Leave, transpiler todo");`);
  }

}