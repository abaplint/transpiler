import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {SourceTranspiler} from "../expressions";
import {Traversal} from "../traversal";

export class ReplaceTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {

    const sources: string[] = [];
    for (const s of node.findDirectExpressions(abaplint.Expressions.Source)) {
      sources.push(new SourceTranspiler().transpile(s, traversal));
    }

    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target));

    return "abap.statements.replace(" + target + ", " + sources.join(", ") + ");";
  }

}