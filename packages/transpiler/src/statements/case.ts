import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {SourceTranspiler} from "../expressions";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class CaseTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const source = new SourceTranspiler(true).transpile(node.findDirectExpression(abaplint.Expressions.Source)!, traversal);
    return new Chunk()
      .append("switch (", node, traversal)
      .appendChunk(source)
      .append(") {", node.getLastToken(), traversal);
  }

}