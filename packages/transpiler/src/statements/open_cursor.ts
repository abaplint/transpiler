import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {findConnection} from "./insert_database";

export class OpenCursorTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {

    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.SQLTarget)).getCode();
    const selectExpression = node.findDirectExpression(abaplint.Expressions.Select);

    let select = "SELECT ";
    select += traversal.traverse(selectExpression?.findDirectExpression(abaplint.Expressions.SQLFieldList)).getCode() + " ";
    select += traversal.traverse(selectExpression?.findDirectExpression(abaplint.Expressions.SQLFrom)).getCode();

    const cond = selectExpression?.findDirectExpression(abaplint.Expressions.SQLCond);
    if (cond) {
      select += "WHERE " + traversal.traverse(node).getCode();
    }

    const orderBy = selectExpression?.findDirectExpression(abaplint.Expressions.SQLOrderBy);
    if (orderBy) {
      select += "ORDER BY " + traversal.traverse(node).getCode();
    }

    const options: string[] = [];
    const connection = node.findDirectExpression(abaplint.Expressions.DatabaseConnection);
    if (connection) {
      const con = findConnection(connection);
      options.push(`"connection": "${con}"`);
    }

    return new Chunk().append(`await abap.statements.openCursor(${target}, "${select}", {${options.join(", ")}});`, node, traversal);
  }

}