import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {SourceTranspiler} from "../expressions";

export class TranslateTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {

    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target));

    let type = "";
    if (node.findDirectTokenByText("UPPER")) {
      type = `"UPPER"`;
    } else if (node.findDirectTokenByText("LOWER")) {
      type = `"LOWER"`;
    } else {
      const s = node.findDirectExpression(abaplint.Expressions.Source);
      if (s) {
        type = new SourceTranspiler(true).transpile(s, traversal);
      } else {
        throw new Error("TranslateTranspiler, Source expression not found");
      }
    }

    return "abap.statements.translate(" + target + ", " + type + ");";
  }

}