import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";

export class IfTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    const cond = traversal.traverse(node.findFirstExpression(abaplint.Expressions.Cond));
    return "if (" + cond + ") {";
  }

}