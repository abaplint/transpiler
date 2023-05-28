import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {DatabaseTableTranspiler} from "../expressions";

export class ModifyDatabaseTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const dbtab = node.findFirstExpression(abaplint.Expressions.DatabaseTable);
    if (dbtab === undefined) {
      throw new Error("internal error, ModifyDatabaseTranspiler");
    }
    const table = new DatabaseTableTranspiler(false).transpile(dbtab, traversal);

    const options: string[] = [];

    const tab = node.findExpressionAfterToken("TABLE");
    if (tab) {
      const ttab = traversal.traverse(tab);
      options.push(`"table": ` + ttab.getCode());
    }

    const from = node.findExpressionAfterToken("FROM");
    if (from && from.get() instanceof abaplint.Expressions.SQLSource) {
      const tvalues = traversal.traverse(from);
      options.push(`"values": ` + tvalues.getCode());
    }

    return new Chunk(`await abap.statements.modifyDatabase(${table.getCode()}, {${options.join(", ")}});`);
  }

}