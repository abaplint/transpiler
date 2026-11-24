import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class FormatTranspiler implements IStatementTranspiler {

  public transpile(_node: abaplint.Nodes.StatementNode, _traversal: Traversal): Chunk {
    // FORMAT only sets how list output looks (colors, intensity, inverse, hotspot, input fields, frames),
    // it does not change the text of the list
    return new Chunk("");
  }

}
