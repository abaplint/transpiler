import * as abaplint from "@abaplint/core";
import {Chunk} from "../chunk";
import {Traversal} from "../traversal";
import {IStatementTranspiler} from "./_statement_transpiler";

export class SkipTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const source = node.findDirectExpression(abaplint.Expressions.Source);
    if (source === undefined) {
      return new Chunk().append("abap.statements.skip();", node, traversal);
    }

    const option = node.concatTokens().toUpperCase().includes("TO LINE") ? "toLine" : "lines";
    return new Chunk().append("abap.statements.skip({" + option + ": ", node, traversal)
      .appendChunk(traversal.traverse(source))
      .append("});", node.getLastToken(), traversal);
  }

}
