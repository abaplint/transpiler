import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class GetBitTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const sources = node.findDirectExpressions(abaplint.Expressions.Source);
    const source0 = traversal.traverse(sources[0]).getCode();
    const source1 = traversal.traverse(sources[1]).getCode();
    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target)).getCode();

    return new Chunk("abap.statements.getBit(" + source0 + ", " + source1 + ", " + target + ");");
  }

}