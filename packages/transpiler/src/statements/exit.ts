import * as abaplint from "@abaplint/core";
import {Chunk} from "../chunk.js";
import {Traversal} from "../traversal.js";
import {ReturnTranspiler} from "./return.js";
import {IStatementTranspiler} from "./_statement_transpiler.js";

export class ExitTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    if (traversal.isInsideLoop(node)) {
      return new Chunk().append("break;", node, traversal);
    } else {
      return new ReturnTranspiler().transpile(node, traversal);
    }
  }

}