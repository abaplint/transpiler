import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class MethodCallBodyTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const ret = new Chunk();

    for (const c of node.getChildren()) {
      if (c instanceof Nodes.TokenNode) {
        // PARAMETER-TABLE
        continue;
      } else {
        ret.appendChunk(traversal.traverse(c));
      }
    }

    return ret;
  }

}