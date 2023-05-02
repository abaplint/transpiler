import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler.js";
import {SourceTranspiler} from "./index.js";
import {Traversal} from "../traversal.js";
import {Chunk} from "../chunk.js";

export class SimpleSource2Transpiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    return new SourceTranspiler().transpile(node, traversal);
  }

}