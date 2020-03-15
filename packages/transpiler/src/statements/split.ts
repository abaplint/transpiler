import * as abaplint from "abaplint";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";

export class SplitTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {

    const sources = node.findDirectExpressions(abaplint.Expressions.Source);

    const source = traversal.traverse(sources[0]);
    const at = traversal.traverse(sources[1]);

    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target));

    return "abap.statements.split({source: " + source + ", at: " + at + ", target: " + target + "});";
  }

}