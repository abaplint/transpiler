import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {SourceTranspiler} from "./source";
import {Chunk} from "../chunk";

export class SimpleSource4Transpiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    return new SourceTranspiler().transpile(node, traversal);
  }

}