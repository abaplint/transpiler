import {Expressions, Nodes} from "abaplint";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {SourceTranspiler, CompareOperatorTranspiler} from ".";
import {Traversal} from "../traversal";

export class CompareTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): string {
// todo, this is not correct
    const sources = node.findDirectExpressions(Expressions.Source);
    if (sources.length >= 2) {
      const operator = new CompareOperatorTranspiler().transpile(node.findFirstExpression(Expressions.CompareOperator)!);
      const s0 = new SourceTranspiler().transpile(sources[0], traversal);
      const s1 = new SourceTranspiler().transpile(sources[1], traversal);
      return "abap.compare." + operator + "(" + s0 + ", " + s1 + ")";
    }

    return "Compare, todo";
  }

}