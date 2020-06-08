import {Nodes, Expressions} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";

export class MethodCallTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): string {

    let ret = node.findDirectExpression(Expressions.MethodName)!.getFirstToken().getStr();
    if (ret === "lines" || ret === "strlen" || ret === "xstrlen") { // todo, this is wrong
      ret = "abap.builtin." + ret + "(";
    } else {
      ret = ret + "(";
    }

    const step = node.findDirectExpression(Expressions.MethodCallParam);
    if (step === undefined) {
      throw new Error("MethodCallTranspiler, unexpected node");
    }

    const source = step.findDirectExpression(Expressions.Source);
    if (source) {
      ret = ret + traversal.traverse(source);
    }
    const parameters = step.findDirectExpression(Expressions.ParameterListS);
    if (parameters) {
      ret = ret + traversal.traverse(parameters);
    }

    return ret + ")";
  }

}