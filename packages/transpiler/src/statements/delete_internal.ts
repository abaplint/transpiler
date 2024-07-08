import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class DeleteInternalTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target)).getCode();
    const concat = node.concatTokens().toUpperCase();

    const extra: string[] = [];

    const componentCond = node.findDirectExpression(abaplint.Expressions.ComponentCond);
    if (componentCond) {
      // todo, replacing "await" is a hack
      extra.push("where: " + traversal.traverse(componentCond).getCode().replace("await ", ""));
    }

    const componentCompare = node.findDirectExpression(abaplint.Expressions.ComponentCompare);
    if (componentCompare) {
      // todo: this can be optimized, WITH TABLE KEY
      extra.push("where: " + traversal.traverse(componentCompare).getCode());
    }

// todo, this is not completely correct, fields might have the name ADJACENT
// comparisons should be on table key unless other is specified, but we're unaware
    if (node.findDirectTokenByText("ADJACENT")) {
      extra.push("adjacent: true");
      if (node.findDirectTokenByText("COMPARING") && !concat.includes("COMPARING ALL FIELDS")) {
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
      if (concat.startsWith("DELETE TABLE ") === true) {
        extra.push("fromValue: " + traversal.traverse(from).getCode());
      } else {
        extra.push("from: " + traversal.traverse(from).getCode());
      }
    }

    const to = node.findExpressionAfterToken("TO");
    if (to) {
      extra.push("to: " + traversal.traverse(to).getCode());
    }

    let blah = "";
    if (extra.length > 0) {
      blah = ",{" + extra.join(",") + "}";
    }

    return new Chunk("await abap.statements.deleteInternal(" + target + blah + ");");
  }

}