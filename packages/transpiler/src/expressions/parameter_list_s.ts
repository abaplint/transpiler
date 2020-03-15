import {Nodes} from "abaplint";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {ParameterSTranspiler} from ".";
import {Traversal} from "../traversal";

export class ParameterListSTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): string {
    const parameters: string[] = [];

    for (const c of node.getChildren()) {
      if (c instanceof Nodes.ExpressionNode) {
        parameters.push(new ParameterSTranspiler().transpile(c, traversal));
      }
    }

    return "{" + parameters.join(", ") + "}";
  }

}