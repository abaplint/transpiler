import * as abaplint from "@abaplint/core";
import {Nodes, Expressions} from "@abaplint/core";
import {Chunk} from "../chunk";
import {Traversal} from "../traversal";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {ParameterListSTranspiler} from "./parameter_list_s";

export class MethodCallParamTranspiler implements IExpressionTranspiler {
  private readonly m: abaplint.Types.MethodDefinition | undefined;
  private readonly suppliedReturning: string | undefined;

  /** On a real system "RETURNING IS SUPPLIED" is true exactly when the method is called
   * functionally, so the caller has to say so.
   * @param m                 the resolved method definition
   * @param suppliedReturning name of the RETURNING parameter to flag as supplied,
   *                          ie. the call is functional and the value is consumed */
  public constructor(m?: abaplint.Types.MethodDefinition, suppliedReturning?: string) {
    this.m = m;
    this.suppliedReturning = suppliedReturning;
  }

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    let name = "";
    if (!(node.get() instanceof Expressions.MethodCallParam)) {
      throw new Error("MethodCallParam, unexpected node, " + node?.get().constructor.name);
    }

    // the RETURNING parameter is passed along when the call is functional, see the constructor
    const extra = this.suppliedReturning === undefined ? "" : this.suppliedReturning + ": 1";

    const source = node.findDirectExpression(Expressions.Source);
    if (source) {
      const def = this.m?.getParameters().getDefaultImporting()?.toLowerCase();
      if (this.m === undefined || def === undefined) {
        // the input is not an object, so there is nowhere to put the RETURNING flag
        return traversal.traverse(source);
      } else {
        return new Chunk()
          .appendString("{" + def + ": ")
          .appendChunk(traversal.traverse(source))
          .appendString(extra === "" ? "}" : ", " + extra + "}");
      }
    }

    const parameters = node.findDirectExpression(Expressions.ParameterListS);
    if (parameters) {
      return new ParameterListSTranspiler(extra).transpile(parameters, traversal);
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

    if (extra === "") {
      return new Chunk(name);
    } else if (name === "") {
      return new Chunk("{" + extra + "}");
    }
    return new Chunk(name.replace(/}$/, ", " + extra + "}"));
  }

}