import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";

export class DeleteInternalTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    const target = traversal.traverse(node.findFirstExpression(abaplint.Expressions.Target));

    const extra: string[] = [];
    const where = node.findFirstExpression(abaplint.Expressions.ComponentCond);
    if (where) {
      extra.push("where: " + traversal.traverse(where));
    }

// todo, this is not completely correct, fields might have the name ADJACENT
    if (node.findDirectTokenByText("ADJACENT")) {
      extra.push("adjacent: true");
      if (node.findDirectTokenByText("COMPARING")) {
        const comparing = node.findAllExpressions(abaplint.Expressions.FieldSub);
        if (comparing) {
          const compareFields = comparing.map(i => i.getFirstToken().getStr()).join(",");
          extra.push("comparing: '" + compareFields + "'");
        }
      }
    }

    const index = node.findExpressionAfterToken("INDEX");
    if (index) {
      extra.push("index: " + traversal.traverse(index));
    }

    const from = node.findExpressionAfterToken("FROM");
    if (from && node.findDirectTokenByText("ADJACENT") === undefined) {
      extra.push("from: " + traversal.traverse(from));
    }

    const to = node.findExpressionAfterToken("TO");
    if (to) {
      extra.push("to: " + traversal.traverse(to));
    }

    let concat = "";
    if (extra.length > 0) {
      concat = ",{" + extra.join(",") + "}";
    }

    return "abap.statements.deleteInternal(" + target + concat + ");";
  }

}