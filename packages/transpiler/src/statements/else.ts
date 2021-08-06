import * as abaplint from "@abaplint/core";
import {Chunk} from "../chunk";
import {Traversal} from "../traversal";
import {IStatementTranspiler} from "./_statement_transpiler";

export class ElseTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    return new Chunk().append("} else {", node, traversal);
  }

}