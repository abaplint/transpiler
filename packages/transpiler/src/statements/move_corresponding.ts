import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";

export class MoveCorrespondingTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target));
    const source = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Source));

    const ret = `abap.statements.moveCorresponding(${source}, ${target});`;

    return ret;
  }

}