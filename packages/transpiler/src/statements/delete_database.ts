import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class DeleteDatabaseTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const table = traversal.traverse(node.findFirstExpression(abaplint.Expressions.DatabaseTable));

    const options: string[] = [];

    const tab = node.findExpressionAfterToken("TABLE");
    if (tab) {
      const ttab = traversal.traverse(tab);
      options.push(`"table": ` + ttab.getCode());
    }

    const w = node.findExpressionAfterToken("WHERE");
    if (w && w.get() instanceof abaplint.Expressions.SQLCond) {
      const ttab = traversal.traverse(w);
      options.push(`"where": "` + ttab.getCode() + `"`);
    }

    return new Chunk(`await abap.statements.deleteDatabase(${table.getCode()}, {${options.join(", ")}});`);
  }

}