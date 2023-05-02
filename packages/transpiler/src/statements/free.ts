import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler.js";
import {Traversal} from "../traversal.js";
import {ClearTranspiler} from "./clear.js";
import {Chunk} from "../chunk.js";

export class FreeTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    return new ClearTranspiler().transpile(node, traversal);
  }

}