import {Nodes} from "@abaplint/core";
import {Chunk} from "../chunk";
import {Traversal} from "../traversal";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {FieldChainTranspiler} from "./field_chain";

export class TargetFieldTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    return new FieldChainTranspiler().transpile(node, traversal);
  }

}