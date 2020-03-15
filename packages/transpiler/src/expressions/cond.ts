import {Expressions, Nodes} from "abaplint";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {CompareTranspiler} from ".";
import {Traversal} from "../traversal";

export class CondTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): string {
// todo, this is not correct
    const int = new CompareTranspiler().transpile(node.findFirstExpression(Expressions.Compare)!, traversal);
    return int;
  }

}