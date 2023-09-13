import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class OpenCursorTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {

    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.SQLTarget)).getCode();

    node.findDirectExpression(abaplint.Expressions.Select);
    /*
    SQLFieldList
    SQLCond
    SQLFrom
    SQLOrderBy
    */

    return new Chunk().append(`await abap.statements.openCursor(${target});`, node, traversal);
  }

}