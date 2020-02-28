import {Expressions, Nodes} from "abaplint";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {SourceTranspiler, CompareOperatorTranspiler} from ".";

export class CompareTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode): string {
// todo, this is not correct
    const sources = node.findDirectExpressions(Expressions.Source);
    const operator = new CompareOperatorTranspiler().transpile(node.findFirstExpression(Expressions.CompareOperator)!);

    const s0 = new SourceTranspiler().transpile(sources[0]);
    const s1 = new SourceTranspiler().transpile(sources[1]);

    return s0 + "." + operator + "(" + s1 + ")";
  }

}