import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";

export class TranslateTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {

    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target));

    let type = "";
    if (node.findDirectTokenByText("UPPER")) {
      type = `"UPPER"`;
    } else if (node.findDirectTokenByText("LOWER")) {
      type = `"LOWER"`;
    }

    return "abap.statements.translate(" + target + ", " + type + ");";
  }

}