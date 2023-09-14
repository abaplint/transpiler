import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class CloseCursorTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const cursor = traversal.traverse(node.findFirstExpression(abaplint.Expressions.SQLSourceSimple)).getCode();
    return new Chunk().append(`await abap.statements.closeCursor(${cursor});`, node, traversal);
  }

}