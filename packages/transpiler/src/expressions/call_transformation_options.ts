import {Nodes} from "@abaplint/core";
import * as abaplint from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class CallTransformationOptionsTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const res = new Chunk().appendString("{");

    for (const c of node.getChildren()) {
      if (c.get() instanceof abaplint.Expressions.Field || c.get() instanceof abaplint.Expressions.Integer) {
        if (res.getCode() !== "{") {
          res.appendString(",");
        }
        res.appendString(c.concatTokens());
      } else if (c.get() instanceof abaplint.Expressions.Source) {
        res.appendChunk(traversal.traverse(c));
      } else if (c.concatTokens() === "=") {
        res.appendString(":");
      } else {
        throw new Error("CallTransformationOptionsTranspiler, unexpected node");
      }
    }
    res.appendString("}");

    return res;
  }

}