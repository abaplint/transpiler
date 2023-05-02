import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler.js";
import {Traversal} from "../traversal.js";
import {Chunk} from "../chunk.js";

export class MoveCorrespondingTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target));
    const source = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Source));

    return new Chunk().append("abap.statements.moveCorresponding(", node, traversal)
      .join([source, target])
      .append(");", node.getLastToken(), traversal);
  }

}