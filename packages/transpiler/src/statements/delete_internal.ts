import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class DeleteInternalTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const target = traversal.traverse(node.findFirstExpression(abaplint.Expressions.Target)).getCode();

    const extra: string[] = [];
    const where = node.findFirstExpression(abaplint.Expressions.ComponentCond);
    if (where) {
      extra.push("where: " + traversal.traverse(where).getCode());
    }

// todo, this is not completely correct, fields might have the name ADJACENT
// comparisons should be on table key unless other is specified, but we're unaware
    if (node.findDirectTokenByText("ADJACENT")) {
      extra.push("adjacent: true");
      if (node.findDirectTokenByText("COMPARING") && !node.concatTokens().toUpperCase().includes("COMPARING ALL FIELDS")) {
        const comparing = node.findAllExpressions(abaplint.Expressions.FieldSub);
        if (comparing) {
          const compareFields = comparing.map(i => "'" + i.getFirstToken().getStr() + "'").join(",");
          extra.push("comparing: [" + compareFields + "]");
        }
      }
    }

    const index = node.findExpressionAfterToken("INDEX");
    if (index) {
      extra.push("index: " + traversal.traverse(index).getCode());
    }

    const from = node.findExpressionAfterToken("FROM");
    if (from && node.findDirectTokenByText("ADJACENT") === undefined) {
      extra.push("from: " + traversal.traverse(from).getCode());
    }

    const to = node.findExpressionAfterToken("TO");
    if (to) {
      extra.push("to: " + traversal.traverse(to).getCode());
    }

    let concat = "";
    if (extra.length > 0) {
      concat = ",{" + extra.join(",") + "}";
    }

    return new Chunk("abap.statements.deleteInternal(" + target + concat + ");");
  }

}