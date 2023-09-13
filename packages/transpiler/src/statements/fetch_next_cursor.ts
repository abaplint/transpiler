import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class FetchNextCursorTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {

    const cursor = traversal.traverse(node.findFirstExpression(abaplint.Expressions.SQLSourceSimple)).getCode();
    // todo: APPENDING and CORRESPONDING FIELDS
    const target = traversal.traverse(node.findFirstExpression(abaplint.Expressions.SQLTarget)).getCode();

    let packageSize = "";
    const siz = node.findExpressionAfterToken("SIZE");
    if (siz) {
      packageSize = ", " + traversal.traverse(siz).getCode();
    }

    return new Chunk().append(`await abap.statements.fetchNextCursor(${cursor}, ${target}${packageSize});`, node, traversal);
  }

}