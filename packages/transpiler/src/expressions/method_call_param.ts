import * as abaplint from "@abaplint/core";
import {Nodes, Expressions} from "@abaplint/core";
import {Chunk} from "../chunk";
import {Traversal} from "../traversal";
import {IExpressionTranspiler} from "./_expression_transpiler";

export class MethodCallParamTranspiler implements IExpressionTranspiler {
  private readonly m: abaplint.Types.MethodDefinition | undefined;
  private readonly suppliedReturning: string | undefined;

  /** @param m                 the resolved method definition
   *  @param suppliedReturning name of the RETURNING parameter to flag as supplied,
   *                           ie. the call is functional and the value is consumed */
  public constructor(m?: abaplint.Types.MethodDefinition, suppliedReturning?: string) {
    this.m = m;
    this.suppliedReturning = suppliedReturning;
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
        // the input is not an object, so there is nowhere to put the RETURNING flag
        return traversal.traverse(source);
      } else {
        return this.addReturning(new Chunk()
          .appendString("{" + def + ": ")
          .appendChunk(traversal.traverse(source))
          .appendString("}"));
      }
    }

    const parameters = node.findDirectExpression(Expressions.ParameterListS);
    if (parameters) {
      return this.addReturning(traversal.traverse(parameters));
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

    if (name === "") {
      return this.suppliedReturning === undefined
        ? new Chunk(name)
        : new Chunk("{" + this.suppliedReturning + ": 1}");
    }

    return this.addReturning(new Chunk(name));
  }

/////////////////////////////

  /** On a real system "RETURNING IS SUPPLIED" is true exactly when the method is
   * called functionally, so the caller has to say so: the returning parameter is
   * added to the input object when, and only when, the value is consumed */
  private addReturning(chunk: Chunk): Chunk {
    if (this.suppliedReturning === undefined) {
      return chunk;
    }
    return chunk.appendObjectField(this.suppliedReturning + ": 1");
  }

}