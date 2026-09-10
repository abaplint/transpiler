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
      const tvalues = traversal.traverseWithTableContext(dbtab.concatTokens(), from);
      options.push(`"values": ` + tvalues.getCode());
    }

    const fromTable = node.findExpressionAfterToken("TABLE");
    if (fromTable && fromTable.get() instanceof abaplint.Expressions.SQLSource) {
      const tvalues = traversal.traverse(fromTable);
      options.push(`"table": ` + tvalues.getCode());
    }

    const connection = node.findDirectExpression(abaplint.Expressions.DatabaseConnection);
    if (connection) {
      const con = findConnection(connection, traversal);
      options.push(`"connection": ${con}`);
    }

    return new Chunk(`await abap.statements.insertDatabase(${table.getCode()}, {${options.join(", ")}});`);
  }
}

export function findConnection(connection: abaplint.Nodes.ExpressionNode, traversal: Traversal): string {
  const dynamic = connection.findDirectExpression(abaplint.Expressions.Dynamic);
  if (dynamic) {
    const field = dynamic.findFirstExpression(abaplint.Expressions.FieldChain);
    if (field === undefined) {
      throw new Error("findConnection: dynamic connection name not found");
    }
    const value = traversal.traverse(field).getCode();
    return `${value}.get().trimEnd().toUpperCase()`;
  }

  let con = connection.getLastToken().getStr().toUpperCase();
  if (con.startsWith("'") && con.endsWith("'")) {
    con = con.substring(1, con.length - 1).replace(/''/g, "'");
  }
  if (con === "DEFAULT_") {
    // todo, workaround for replacing of keywords,
    con = "DEFAULT";
  }
  return JSON.stringify(con);
}
