import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler.js";
import {Traversal} from "../traversal.js";
import {Chunk} from "../chunk.js";

export class FieldSymbolTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    let name = node.concatTokens();
    name = name.replace("<", "fs_");
    name = name.replace(">", "_");
    return new Chunk().append(name, node, traversal);
  }

}