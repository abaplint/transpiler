import {Nodes} from "@abaplint/core";
import * as abaplint from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler.js";
import {Traversal} from "../traversal.js";
import {Chunk} from "../chunk.js";

export class FunctionExportingTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const parameters: Chunk[] = [];

    let chunk = new Chunk();
    for (const parameter of node.getChildren()) {
      for (const child of parameter.getChildren()) {
        if (child.getFirstToken().getStr() === "=") {
          chunk.appendString(": ");
        } else if (child.get() instanceof abaplint.Expressions.ParameterName) {
          chunk = new Chunk();
          chunk.appendChunk(traversal.traverse(child));
        } else {
          chunk.appendChunk(traversal.traverse(child));
          parameters.push(chunk);
        }
      }
    }

    return new Chunk().appendString("{").join(parameters).appendString("}");
  }

}