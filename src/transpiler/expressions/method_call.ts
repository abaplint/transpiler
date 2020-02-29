import {Nodes, Expressions} from "abaplint";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {SourceTranspiler} from ".";

export class MethodCallTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode): string {

    const name = node.findDirectExpression(Expressions.MethodName)!.getFirstToken().getStr();

    let ret = "abap.builtin." + name + "(";

    const source = node.findDirectExpression(Expressions.Source);
    if (source) {
      ret = ret + new SourceTranspiler().transpile(source);
    }

    return ret + ")";
  }

}