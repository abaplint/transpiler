import * as abaplint from "@abaplint/core";
import {Nodes, Expressions} from "@abaplint/core";
import {Chunk} from "../chunk";
import {Traversal} from "../traversal";
import {IExpressionTranspiler} from "./_expression_transpiler";

export class MethodCallParam implements IExpressionTranspiler {
  private readonly m: abaplint.Types.MethodDefinition | undefined;

  public constructor(m?: abaplint.Types.MethodDefinition) {
    this.m = m;
  }

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    let name = "";

    if (!(node.get() instanceof Expressions.MethodCallParam)) {
      throw new Error("MethodCallParam, unexpected node, " + node?.get().constructor.name);
    }

    const source = node.findDirectExpression(Expressions.Source);
    if (source) {
      const def = this.m?.getParameters().getDefaultImporting()?.toLowerCase();
      if (this.m === undefined || def === undefined) {
        return traversal.traverse(source);
      } else {
        return new Chunk()
          .appendString("{" + def + ": ")
          .appendChunk(traversal.traverse(source))
          .appendString("}");
      }
    }

    const parameters = node.findDirectExpression(Expressions.ParameterListS);
    if (parameters) {
      return traversal.traverse(parameters);
    } else {
      const params = node.findDirectExpression(Expressions.MethodParameters);
      if (params) {
        const s = params.findDirectExpression(Expressions.ParameterListS);
        if (s) {
          name += traversal.traverse(s).getCode();
        }
        for (const t of params.findDirectExpressions(Expressions.ParameterListT)) {
          name += traversal.traverse(t).getCode();
        }
      }
    }

    name = name.replace(/}{/g, ", ");

    return new Chunk(name);
  }

}