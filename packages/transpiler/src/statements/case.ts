import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler.js";
import {SourceTranspiler} from "../expressions/index.js";
import {Traversal} from "../traversal.js";
import {Chunk} from "../chunk.js";

export class CaseTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const source = new SourceTranspiler(true).transpile(node.findDirectExpression(abaplint.Expressions.Source)!, traversal);
    return new Chunk()
      .append("switch (", node, traversal)
      .appendChunk(source)
      .append(") {", node.getLastToken(), traversal);
  }

}