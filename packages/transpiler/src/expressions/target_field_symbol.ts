import {Nodes} from "@abaplint/core";
import {Chunk} from "../chunk";
import {Traversal} from "../traversal";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {FieldSymbolTranspiler} from "./field_symbol";

export class TargetFieldSymbolTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    return new FieldSymbolTranspiler().transpile(node, traversal);
  }

}