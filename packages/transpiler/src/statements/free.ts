import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {ClearTranspiler} from "./clear";
import {Chunk} from "../chunk";

export class FreeTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    // FREE of a table with a header line frees the table body, not the header line
    return new ClearTranspiler(true).transpile(node, traversal);
  }

}