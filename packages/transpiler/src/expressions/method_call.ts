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
    if (name === "lines" || name === "strlen" || name === "xstrlen") { // todo, this is wrong, look at MethodReferences instead
      name = "abap.builtin." + name + "(";
    } else {
      name = name + "(";
    }

    const step = node.findDirectExpression(Expressions.MethodCallParam);
    if (step === undefined) {
      throw new Error("MethodCallTranspiler, unexpected node");
    }

    const m = this.findMethodReference(nameToken, traversal.findCurrentScope(nameToken));
    if (m?.name) {
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

    const parameters = step.findFirstExpression(Expressions.ParameterListS);
    if (parameters) {
      name = name + traversal.traverse(parameters);
    }
    for (const t of step.findAllExpressions(Expressions.ParameterListT)) {
      name = name + traversal.traverse(t);
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
      if (r.referenceType !== abaplint.ReferenceType.MethodReference) {
        continue;
      } else if (r.position.getStart().equals(token.getStart())
          && r.resolved instanceof abaplint.Types.MethodDefinition) {

        let name = r.resolved.getName();
        if (r.extra?.ooName && r.extra?.ooType === "INTF") {
          name = r.extra.ooName + "$" + name;
        }

        return {def: r.resolved, name};
      }
    }

    return undefined;
  }

}