import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";

export class ShiftTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {

    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target));

    const options: string[] = [];

    if (node.findDirectTokenByText("LEFT")) {
      options.push("direction: 'LEFT'");
    }

    const leading = node.findExpressionAfterToken("LEADING");
    if (leading) {
      options.push("deletingLeading: " + traversal.traverse(leading));
    }

    const extra = options.length > 0 ? ", {" + options.join(",") + "}" : "";
    return "abap.statements.shift(" + target + extra + ");";
  }

}