import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";

export class FindTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    const options: string[] = [];

    const sources = node.findDirectExpressions(abaplint.Expressions.Source);
    const source0 = traversal.traverse(sources[0]);
    if (node.findDirectTokenByText("REGEX")) {
      options.push("regex: " + source0);
    } else {
      options.push("find: " + source0);
    }
    const source1 = traversal.traverse(sources[1]);

    const off = node.findExpressionAfterToken("OFFSET");
    if (off) {
      options.push("offset: " + traversal.traverse(off));
    }

    const cnt = node.findExpressionAfterToken("COUNT");
    if (cnt) {
      options.push("count: " + traversal.traverse(cnt));
    }

    const len = node.findExpressionAfterToken("LENGTH");
    if (len) {
      options.push("length: " + traversal.traverse(len));
    }

    const firstSubmatch = node.findExpressionAfterToken("SUBMATCHES");
    if (firstSubmatch) {
      const submatches: string[] = [];
      for (const t of node.findDirectExpressions(abaplint.Expressions.Target)) {
        submatches.push(traversal.traverse(t));
      }
      options.push("submatches: [" + submatches.join(",") + "]");
    }

    return "abap.statements.find(" + source1 + ", {" + options.join(", ") + "});";
  }

}