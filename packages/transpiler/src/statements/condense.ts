import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";

export class CondenseTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target));
    const noGaps = node.concatTokens().search(/NO-GAPS.$/) > -1;
    return "abap.statements.condense(" + target + ", {nogaps: " + noGaps + "});";
  }

}