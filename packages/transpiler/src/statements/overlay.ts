import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler.js";
import {Traversal} from "../traversal.js";
import {Chunk} from "../chunk.js";

export class OverlayTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const ret = new Chunk().append("abap.statements.overlay(", node, traversal);

    ret.appendChunk(traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target)));

    for (const s of node.findDirectExpressions(abaplint.Expressions.Source)) {
      ret.appendString(",");
      ret.appendChunk(traversal.traverse(s));
    }

    ret.append(");", node.getLastToken(), traversal);

    return ret;
  }

}