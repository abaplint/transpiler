import {Expressions, Nodes} from "abaplint";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {ComponentCompareTranspiler} from ".";
import {Traversal} from "../traversal";

export class ComponentCondTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): string {
// todo, this is not correct
    const int = new ComponentCompareTranspiler().transpile(node.findFirstExpression(Expressions.ComponentCompare)!, traversal);
    return int;
  }

}