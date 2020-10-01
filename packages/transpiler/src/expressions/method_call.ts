import {Nodes, Expressions} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";

export class MethodCallTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): string {

    let ret = node.findDirectExpression(Expressions.MethodName)!.getFirstToken().getStr();
    if (ret === "lines" || ret === "strlen" || ret === "xstrlen") { // todo, this is wrong, look at methodreferences instead
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

    const parameters = step.findFirstExpression(Expressions.ParameterListS);
    if (parameters) {
      ret = ret + traversal.traverse(parameters);
    }
    for (const t of step.findAllExpressions(Expressions.ParameterListT)) {
      ret = ret + traversal.traverse(t);
    }
    ret = ret.replace(/}{/g, ", ");

    return ret + ")";
  }

}