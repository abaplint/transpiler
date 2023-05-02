import {Nodes, Expressions} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler.js";
import {Traversal} from "../traversal.js";
import {Chunk} from "../chunk.js";

export class MethodParametersTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const ret = new Chunk("{");

    for (const s of node.findAllExpressions(Expressions.ParameterS)) {
      if (ret.getCode() !== "{") {
        ret.appendString(",");
      }
      ret.appendChunk(traversal.traverse(s));
    }

    const receiving = node.findExpressionAfterToken("RECEIVING");

    for (const t of node.findAllExpressions(Expressions.ParameterT)) {
      if (t === receiving) {
        continue;
      }
      if (ret.getCode() !== "{") {
        ret.appendString(",");
      }
      ret.appendChunk(traversal.traverse(t));
    }

    ret.appendString("}");

    if (ret.getCode() === "{}") {
      return new Chunk();
    } else {
      return ret;
    }
  }

}