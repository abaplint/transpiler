import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {DatabaseTableTranspiler} from "../expressions";

export class InsertDatabaseTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const dbtab = node.findFirstExpression(abaplint.Expressions.DatabaseTable);
    if (dbtab === undefined) {
      throw new Error("internal error, InsertDatabaseTranspiler");
    }
    const table = new DatabaseTableTranspiler(false).transpile(dbtab, traversal);

    const options: string[] = [];

    const values = node.findExpressionAfterToken("VALUES");
    if (values) {
      const tvalues = traversal.traverse(values);
      options.push(`"values": ` + tvalues.getCode());
    }

    const from = node.findExpressionAfterToken("FROM");
    if (from && from.get() instanceof abaplint.Expressions.SQLSource) {
      const tvalues = traversal.traverse(from);
      options.push(`"values": ` + tvalues.getCode());
    }

    const fromTable = node.findExpressionAfterToken("TABLE");
    if (fromTable && fromTable.get() instanceof abaplint.Expressions.SQLSource) {
      const tvalues = traversal.traverse(fromTable);
      options.push(`"table": ` + tvalues.getCode());
    }

    return new Chunk(`await abap.statements.insertDatabase(${table.getCode()}, {${options.join(", ")}});`);
  }

}