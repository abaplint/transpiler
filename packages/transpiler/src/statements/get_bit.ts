import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";

export class GetBitTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    const sources = node.findDirectExpressions(abaplint.Expressions.Source);
    const source0 = traversal.traverse(sources[0]);
    const source1 = traversal.traverse(sources[1]);
    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target));

    return "abap.statements.getBit(" + source0 + ", " + source1 + ", " + target + ");";
  }

}