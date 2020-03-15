import {Nodes} from "abaplint";
import * as abaplint from "abaplint";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {ParameterSTranspiler} from ".";

export class ParameterListSTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, spaghetti: abaplint.SpaghettiScope, filename: string): string {
    const parameters: string[] = [];

    for (const c of node.getChildren()) {
      if (c instanceof Nodes.ExpressionNode) {
        parameters.push(new ParameterSTranspiler().transpile(c, spaghetti, filename));
      }
    }

    return "{" + parameters.join(", ") + "}";
  }

}