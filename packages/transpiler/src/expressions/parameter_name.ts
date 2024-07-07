import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class ParameterNameTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const nameToken = node.getFirstToken();
    const name = Traversal.escapeNamespace(nameToken.getStr().toLowerCase())!;

    return new Chunk().append(name, nameToken, traversal);
  }

}