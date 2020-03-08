import {Nodes, Expressions} from "abaplint";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {SourceTranspiler, ParameterListSTranspiler} from ".";

export class MethodCallTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode): string {

    let ret = node.findDirectExpression(Expressions.MethodName)!.getFirstToken().getStr();
    if (ret === "lines") { // todo, this is wrong
      ret = "abap.builtin." + ret + "(";
    } else {
      ret = ret + "(";
    }

    const source = node.findDirectExpression(Expressions.Source);
    if (source) {
      ret = ret + new SourceTranspiler().transpile(source);
    }
    const parameters = node.findDirectExpression(Expressions.ParameterListS);
    if (parameters) {
      ret = ret + new ParameterListSTranspiler().transpile(parameters);
    }

    return ret + ")";
  }

}