import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {SourceTranspiler} from "../expressions";

export class ModifyInternalTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {

    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target));

    const extra: string[] = [];

    const index = node.findExpressionAfterToken("INDEX");
    if (index) {
      const s = new SourceTranspiler().transpile(index, traversal);
      extra.push("index: " + s);
    }

    const from = node.findExpressionAfterToken("FROM");
    if (from) {
      const s = new SourceTranspiler().transpile(from, traversal);
      extra.push("from: " + s);
    }

    let concat = "";
    if (extra.length > 0) {
      concat = ",{" + extra.join(",") + "}";
    }

    return "abap.statements.modifyInternal(" + target + concat + ");";
  }

}