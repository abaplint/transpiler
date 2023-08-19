import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class ComputeTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    let source = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Source));

    const targets: Chunk[] = [];
    for (const t of node.findDirectExpressions(abaplint.Expressions.Target)) {
      targets.push(traversal.traverse(t));
    }

    const ret = new Chunk();

    for (const target of targets.reverse()) {
      ret.appendChunk(target)
        .appendString(".set(")
        .appendChunk(source)
        .append(");", node.getLastToken(), traversal);
      source = target;
    }

    return ret;
  }

}