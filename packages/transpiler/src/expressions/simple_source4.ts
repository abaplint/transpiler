import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {SourceTranspiler} from "./source";

export class SimpleSource4Transpiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): string {
    return new SourceTranspiler().transpile(node, traversal);
  }

}