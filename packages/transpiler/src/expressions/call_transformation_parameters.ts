import {Nodes} from "@abaplint/core";
import * as abaplint from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler.js";
import {Traversal} from "../traversal.js";
import {Chunk} from "../chunk.js";

export class CallTransformationParametersTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    if (node.getFirstChild()?.get() instanceof abaplint.Expressions.Dynamic) {
      return new Chunk().append(node.concatTokens(), node, traversal);
    } else {

      const res = new Chunk().appendString("{");

      for (const c of node.getChildren()) {
        if (c.get() instanceof abaplint.Expressions.Field || c.get() instanceof abaplint.Expressions.Integer) {
          if (res.getCode() !== "{") {
            res.appendString(",");
          }
          res.appendString(c.concatTokens());
        } else if (c.get() instanceof abaplint.Expressions.SimpleSource3) {
          res.appendChunk(traversal.traverse(c));
        } else if (c.concatTokens() === "=") {
          res.appendString(":");
        } else {
          throw new Error("CallTransformationParametersTranspiler, unexpected node");
        }
      }
      res.appendString("}");

      return res;
    }
  }

}