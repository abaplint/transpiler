import {Nodes} from "@abaplint/core";
import {FieldSymbolTranspiler} from "./index.js";
import {Chunk} from "../chunk.js";
import {Traversal} from "../traversal.js";
import {IExpressionTranspiler} from "./_expression_transpiler.js";

export class SourceFieldSymbolTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    return new FieldSymbolTranspiler().transpile(node, traversal);
  }

}