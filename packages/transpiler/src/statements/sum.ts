import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class SumTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const level = traversal.controlLevels[traversal.controlLevels.length - 1];
    if (level === undefined) {
      // eg. in a FORM called from the LOOP, the LOOP is not known here
      return new Chunk(`throw new Error("SUM outside its LOOP, not supported, transpiler");`);
    } else if (level.assigning === true) {
      return new Chunk().append(`throw new Error("SUM_NO_ASSIGNING");`, node, traversal);
    } else if (level.workArea === undefined) {
      return new Chunk(`throw new Error("SUM without INTO, not supported, transpiler");`);
    }
    // the sums of the current control level, of all rows in AT FIRST, AT LAST and outside AT
    const key = level.key === undefined ? "" : ", " + level.key;
    return new Chunk()
      .append(level.collect, node, traversal)
      .appendString(`abap.statements.sum(${level.workArea}, ${level.rows}, ${level.row}${key});`);
  }

}
