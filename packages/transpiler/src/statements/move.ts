import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler.js";
import {Traversal} from "../traversal.js";
import {Chunk} from "../chunk.js";

export class MoveTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    let source = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Source));

    const targets: Chunk[] = [];
    for (const t of node.findDirectExpressions(abaplint.Expressions.Target)) {
      targets.push(traversal.traverse(t));
    }

    const ret = new Chunk();
    const second = node.getChildren()[1]?.concatTokens();
    if (second === "?=") {
      ret.appendString("await abap.statements.cast(")
        .appendChunk(targets[0])
        .appendString(", ")
        .appendChunk(source)
        .append(");", node.getLastToken(), traversal);
    } else {
      for (const target of targets.reverse()) {
        ret.appendChunk(target)
          .appendString(".set(")
          .appendChunk(source)
          .append(");", node.getLastToken(), traversal);
        source = target;
      }
    }

    return ret;
  }

}