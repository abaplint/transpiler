import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {FieldSymbolTranspiler, SourceTranspiler} from "../expressions";
import {Traversal} from "../traversal";

export class AssignTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    const sources = node.findDirectExpressions(abaplint.Expressions.Source).map(e => new SourceTranspiler(false).transpile(e, traversal));
    const fs = new FieldSymbolTranspiler().transpile(node.findDirectExpression(abaplint.Expressions.FSTarget)!, traversal);

    const options: string[] = [];

    const concat = node.concatTokens();
    if (concat.startsWith("ASSIGN COMPONENT ")) {
      options.push("component: " + sources.shift());
    }

    options.push("target: " + fs);
    options.push("source: " + sources.pop());

    if (concat.endsWith(" CASTING.")) {
      options.push("casting: true");
    }

    return "abap.statements.assign({" + options.join(", ") + "});";
  }

}