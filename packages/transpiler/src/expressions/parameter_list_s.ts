import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler.js";
import {Traversal} from "../traversal.js";
import {Chunk} from "../chunk.js";

export class ParameterListSTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const parameters: Chunk[] = [];

    for (const c of node.getChildren()) {
      if (c instanceof Nodes.ExpressionNode) {
        parameters.push(traversal.traverse(c));
      }
    }

    return new Chunk().appendString("{").join(parameters).appendString("}");
  }

}