import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler.js";
import {Traversal} from "../traversal.js";
import {Chunk} from "../chunk.js";

export class ParameterNameTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const nameToken = node.getFirstToken();
    const name = nameToken.getStr().toLowerCase();

    return new Chunk().append(name, nameToken, traversal);
  }

}