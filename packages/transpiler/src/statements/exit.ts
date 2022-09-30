import * as abaplint from "@abaplint/core";
import {Chunk} from "../chunk";
import {Traversal} from "../traversal";
import {ReturnTranspiler} from "./return";
import {IStatementTranspiler} from "./_statement_transpiler";

export class ExitTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    if (traversal.isInsideLoop(node)) {
      return new Chunk().append("break;", node, traversal);
    } else {
      return new ReturnTranspiler().transpile(node, traversal);
    }
  }

}