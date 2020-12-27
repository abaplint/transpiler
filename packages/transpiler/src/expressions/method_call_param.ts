import * as abaplint from "@abaplint/core";
import {Nodes, Expressions} from "@abaplint/core";
import {Traversal} from "../traversal";
import {IExpressionTranspiler} from "./_expression_transpiler";

export class MethodCallParam implements IExpressionTranspiler {
  private readonly m: abaplint.Types.MethodDefinition | undefined;

  public constructor(m?: abaplint.Types.MethodDefinition) {
    this.m = m;
  }

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): string {
    let name = "";

    if (!(node.get() instanceof Expressions.MethodCallParam)) {
      throw new Error("MethodCallParam, unexpected node, " + node?.get().constructor.name);
    }

    const source = node.findDirectExpression(Expressions.Source);
    if (source) {
      const def = this.m?.getParameters().getDefaultImporting()?.toLowerCase();
      if (this.m === undefined || def === undefined) {
        name = name + traversal.traverse(source);
      } else {
        name = name + "{" + def + ": " + traversal.traverse(source) + "}";
      }
    }

    const parameters = node.findDirectExpression(Expressions.ParameterListS);
    if (parameters) {
      name = name + traversal.traverse(parameters);
    } else {
      const params = node.findDirectExpression(Expressions.MethodParameters);
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

    return name;
  }

}