import * as abaplint from "abaplint";
import {IStatementTranspiler} from "./_statement_transpiler";
import {CondTranspiler} from "../expressions";
import {Traversal} from "../traversal";

export class ElseIfTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): string {
    const cond = new CondTranspiler().transpile(node.findFirstExpression(abaplint.Expressions.Cond)!, traversal);
    return "} else if (" + cond + ") {";
  }

}