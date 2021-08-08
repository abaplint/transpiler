import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class MoveTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const source = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Source));
    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target));

    const ret = new Chunk()
      .appendChunk(target)
      .append(".set(", node, traversal)
      .appendChunk(source)
      .append(");", node.getLastToken(), traversal);

    return ret;
  }

}