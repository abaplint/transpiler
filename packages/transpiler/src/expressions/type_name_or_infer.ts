import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class TypeNameOrInfer implements IExpressionTranspiler {

  public transpile(_node: Nodes.ExpressionNode, _traversal: Traversal): Chunk {
    return new Chunk().appendString("TypeNameOrInfer");
  }

}