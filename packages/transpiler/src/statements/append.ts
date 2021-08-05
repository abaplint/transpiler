import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {SourceTranspiler} from "../expressions";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class AppendTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const concat = node.concatTokens();

    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target)).getCode();

    if (concat.toUpperCase().includes("INITIAL LINE")) {
      const found = node.findFirstExpression(abaplint.Expressions.FieldSymbol);
      if (found) {
        const fs = traversal.traverse(found).getCode();
        return new Chunk(fs + ".assign(" + target + ".appendInitial());");
      } else {
        const into = node.findExpressionAfterToken("INTO");
        const ref = traversal.traverse(into).getCode();
        return new Chunk(ref + ".assign(" + target + ".appendInitial());");
      }
    } else {
      const options: string[] = [];

      const s = node.findDirectExpression(abaplint.Expressions.SimpleSource4);
      if (s) {
        options.push("source: " + new SourceTranspiler().transpile(s, traversal).getCode());
      }

      const assigning = node.findExpressionAfterToken("ASSIGNING");
      if (assigning) {
        options.push("assigning: " + traversal.traverse((assigning.findFirstExpression(abaplint.Expressions.FieldSymbol))).getCode());
      }

      if (concat.startsWith("APPEND LINES OF ")) {
        options.push("lines: true");
      }

      options.push("target: " + target);

      return new Chunk("abap.statements.append({" + options.join(", ") + "});");
    }

  }

}