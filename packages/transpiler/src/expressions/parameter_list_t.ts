import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class ParameterListTTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const parameters: string[] = [];

    for (const c of node.getChildren()) {
      if (c instanceof Nodes.ExpressionNode) {
        parameters.push(traversal.traverse(c).getCode());
      }
    }

    return new Chunk("{" + parameters.join(", ") + "}");
  }

}