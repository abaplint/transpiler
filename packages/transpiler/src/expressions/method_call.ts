import {Nodes, Expressions} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {MethodCallParam} from "./method_call_param";

export class MethodCallTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): string {

    const nameToken = node.findDirectExpression(Expressions.MethodName)?.getFirstToken();
    if(nameToken === undefined) {
      throw new Error("MethodCallTranspiler, name not found");
    }

    let name = nameToken.getStr();
    if (traversal.isBuiltinMethod(nameToken)) {
      name = "abap.builtin." + name + "(";
    } else {
      name = name + "(";
    }

    const m = traversal.findMethodReference(nameToken, traversal.findCurrentScope(nameToken));
    if (m?.name && traversal.isBuiltinMethod(nameToken) === false) {
      name = m.name + "(";
    }

    const step = node.findDirectExpression(Expressions.MethodCallParam);
    if (step === undefined) {
      throw new Error("MethodCallTranspiler, unexpected node");
    }

    name += new MethodCallParam().transpile(step, traversal, m?.def);

    return name + ")";
  }

}