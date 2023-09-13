import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class OpenCursorTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {

    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.SQLTarget)).getCode();
    const selectExpression = node.findDirectExpression(abaplint.Expressions.Select);

    let select = "SELECT ";
    select += traversal.traverse(selectExpression?.findDirectExpression(abaplint.Expressions.SQLFieldList)).getCode() + " FROM ";
    select += traversal.traverse(selectExpression?.findDirectExpression(abaplint.Expressions.SQLFrom)).getCode();

    const cond = selectExpression?.findDirectExpression(abaplint.Expressions.SQLCond);
    if (cond) {
      select += "WHERE " + traversal.traverse(node).getCode();
    }

    const orderBy = selectExpression?.findDirectExpression(abaplint.Expressions.SQLOrderBy);
    if (orderBy) {
      select += "ORDER BY " + traversal.traverse(node).getCode();
    }

    return new Chunk().append(`await abap.statements.openCursor(${target}, "${select}");`, node, traversal);
  }

}