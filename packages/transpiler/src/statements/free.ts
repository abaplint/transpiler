import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {ClearTranspiler} from "./clear";
import {Chunk} from "../chunk";

export class FreeTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    return new ClearTranspiler().transpile(node, traversal);
  }

}