import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";

export class MoveTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    const source = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Source));
    const target = traversal.traverse(node.findDirectExpression(abaplint.Expressions.Target));
    return target + ".set(" + source + ");";
  }

}