import {Expressions, Nodes} from "abaplint";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";

export class ComponentCompareTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): string {
    const component = traversal.traverse(node.findDirectExpression(Expressions.ComponentChainSimple));
    const compare = traversal.traverse(node.findDirectExpression(Expressions.CompareOperator));
    const source = traversal.traverse(node.findDirectExpression(Expressions.Source));
    return "(i) => {return abap.compare." + compare + "(i." + component + ", " + source + ");}";
  }

}