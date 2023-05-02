import * as abaplint from "@abaplint/core";
import {Chunk} from "../chunk.js";
import {Traversal} from "../traversal.js";
import {IStatementTranspiler} from "./_statement_transpiler.js";

export class EndFormTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    return new Chunk().append("}", node, traversal);
  }

}