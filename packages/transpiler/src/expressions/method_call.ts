import {Nodes, Expressions} from "abaplint";
import * as abaplint from "abaplint";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {SourceTranspiler, ParameterListSTranspiler} from ".";

export class MethodCallTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, spaghetti: abaplint.SpaghettiScope, filename: string): string {

    let ret = node.findDirectExpression(Expressions.MethodName)!.getFirstToken().getStr();
    if (ret === "lines") { // todo, this is wrong
      ret = "abap.builtin." + ret + "(";
    } else {
      ret = ret + "(";
    }

    const source = node.findDirectExpression(Expressions.Source);
    if (source) {
      ret = ret + new SourceTranspiler().transpile(source, spaghetti, filename);
    }
    const parameters = node.findDirectExpression(Expressions.ParameterListS);
    if (parameters) {
      ret = ret + new ParameterListSTranspiler().transpile(parameters, spaghetti, filename);
    }

    return ret + ")";
  }

}