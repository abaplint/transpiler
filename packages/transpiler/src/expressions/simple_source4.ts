import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler.js";
import {Traversal} from "../traversal.js";
import {SourceTranspiler} from "./source.js";
import {Chunk} from "../chunk.js";

export class SimpleSource4Transpiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    return new SourceTranspiler().transpile(node, traversal);
  }

}