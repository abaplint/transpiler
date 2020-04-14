import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";

export class SetBitTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    const sources = node.findDirectExpressions(abaplint.Expressions.Source);
    const source = traversal.traverse(sources[0]);
    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target));
    const value = sources[1] ? traversal.traverse(sources[1]) : undefined;

    if (value) {
      return "abap.statements.setBit(" + source + ", " + target + ", " + value + ");";
    } else {
      return "abap.statements.setBit(" + source + ", " + target + ");";
    }
  }

}