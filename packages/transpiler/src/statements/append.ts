import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {SourceTranspiler} from "../expressions";
import {Traversal} from "../traversal";

export class AppendTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    const options: string[] = [];
    const concat = node.concatTokens();

    const s = node.findDirectExpression(abaplint.Expressions.SimpleSource4);
    if (s) {
      options.push("source: " + new SourceTranspiler().transpile(s, traversal));
    }

    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target));

    if (node.concatTokens().toUpperCase().includes("INITIAL LINE")) {
      const found = node.findFirstExpression(abaplint.Expressions.FieldSymbol);
      if (found) {
        const fs = traversal.traverse(found);
        return fs + ".assign(" + target + ".appendInitial());";
      } else {
        const into = node.findExpressionAfterToken("INTO");
        const ref = traversal.traverse(into);
        return ref + ".assign(" + target + ".appendInitial());";
      }
    } else {
      const assigning = node.findExpressionAfterToken("ASSIGNING");
      if (assigning) {
        options.push("assigning: " + traversal.traverse((assigning.findFirstExpression(abaplint.Expressions.FieldSymbol))));
      }

      if (concat.startsWith("APPEND LINES OF ")) {
        options.push("lines: true");
      }
      options.push("target: " + target);
      return "abap.statements.append({" + options.join(", ") + "});";
    }

  }

}