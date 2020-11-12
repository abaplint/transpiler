import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {SourceTranspiler} from "../expressions";
import {Traversal} from "../traversal";

export class AppendTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    const options: string[] = [];

    const s = node.findDirectExpression(abaplint.Expressions.SimpleSource);
    if (s) {
      options.push("source: " + new SourceTranspiler().transpile(s, traversal));
    }

    if (node.concatTokens().toUpperCase().includes("INITIAL LINE")) {
      options.push("initial: " + traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target)));
      options.push("target: " + traversal.traverse(node.findFirstExpression(abaplint.Expressions.FieldSymbol)));
    } else {
      options.push("target: " + traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target)));
    }

    return "abap.statements.append({" + options.join(", ") + "});";
  }

}