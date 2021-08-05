import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {SourceTranspiler} from ".";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class SimpleSource2Transpiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    return new SourceTranspiler().transpile(node, traversal);
  }

}