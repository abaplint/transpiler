import {Expressions, Nodes} from "abaplint";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {CompareOperatorTranspiler, SourceTranspiler, ComponentChainSimpleTranspiler} from ".";
import {Traversal} from "../traversal";

export class ComponentCompareTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): string {
    const component = new ComponentChainSimpleTranspiler().transpile(node.findDirectExpression(Expressions.ComponentChainSimple)!);
    const compare = new CompareOperatorTranspiler().transpile(node.findDirectExpression(Expressions.CompareOperator)!);
    const source = new SourceTranspiler().transpile(node.findDirectExpression(Expressions.Source)!, traversal);
    return "(i) => {return abap.compare." + compare + "(i." + component + ", " + source + ");}";
  }

}