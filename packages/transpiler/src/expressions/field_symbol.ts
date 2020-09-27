import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";

export class FieldSymbolTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, _traversal: Traversal): string {
    let name = node.concatTokens();
    name = name.replace("<", "fs_");
    name = name.replace(">", "_");
    return name;
  }

}