import {Nodes} from "@abaplint/core";
import {FieldSymbolTranspiler} from ".";
import {Chunk} from "../chunk";
import {Traversal} from "../traversal";
import {IExpressionTranspiler} from "./_expression_transpiler";

export class SourceFieldSymbolChainTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    return new FieldSymbolTranspiler().transpile(node, traversal);
  }

}