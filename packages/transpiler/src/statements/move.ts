import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class MoveTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const source = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Source));

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
      for (const target of targets) {
        ret.appendChunk(target)
          .appendString(".set(")
          .appendChunk(source)
          .append(");", node.getLastToken(), traversal);
      }
    }

    return ret;
  }

}