import * as abaplint from "@abaplint/core";
import {Nodes, Expressions, ISpaghettiScopeNode} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";

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

    const step = node.findDirectExpression(Expressions.MethodCallParam);
    if (step === undefined) {
      throw new Error("MethodCallTranspiler, unexpected node");
    }

    const m = this.findMethodReference(nameToken, traversal.findCurrentScope(nameToken));
    if (m?.name && traversal.isBuiltinMethod(nameToken) === false) {
      name = m.name + "(";
    }

    const source = step.findDirectExpression(Expressions.Source);
    if (source) {
      const def = m?.def?.getParameters().getDefaultImporting()?.toLowerCase();
      if (m === undefined || def === undefined) {
        name = name + traversal.traverse(source);
      } else {
        name = name + "{" + def + ": " + traversal.traverse(source) + "}";
      }
    }

    const parameters = step.findDirectExpression(Expressions.ParameterListS);
    if (parameters) {
      name = name + traversal.traverse(parameters);
    } else {
      const params = step.findDirectExpression(Expressions.MethodParameters);
      if (params) {
        const s = params.findDirectExpression(Expressions.ParameterListS);
        if (s) {
          name += traversal.traverse(s);
        }
        for (const t of params.findDirectExpressions(Expressions.ParameterListT)) {
          name += traversal.traverse(t);
        }
      }
    }

    name = name.replace(/}{/g, ", ");

    return name + ")";
  }

///////////////////

  private findMethodReference(token: abaplint.Token, scope: ISpaghettiScopeNode | undefined):
  undefined | {def: abaplint.Types.MethodDefinition, name: string} {

    if (scope === undefined) {
      return undefined;
    }

    for (const r of scope.getData().references) {
      if (r.referenceType === abaplint.ReferenceType.MethodReference
          && r.position.getStart().equals(token.getStart())
          && r.resolved instanceof abaplint.Types.MethodDefinition) {
        let name = r.resolved.getName();
        if (r.extra?.ooName && r.extra?.ooType === "INTF") {
          name = r.extra.ooName + "$" + name;
        }

        return {def: r.resolved, name};
      } else if (r.referenceType === abaplint.ReferenceType.BuiltinMethodReference
          && r.position.getStart().equals(token.getStart())) {
        const def = r.resolved as abaplint.Types.MethodDefinition;
        const name = def.getName();

        return {def, name};
      }
    }

    return undefined;
  }

}