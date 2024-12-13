import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class FieldSymbolTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    let name = node.concatTokens();
    name = name.replace("<", "fs_");
    name = name.replace(">", "_");
    name = name.replace("[]", "");
    return new Chunk().append(name, node, traversal);
  }

}