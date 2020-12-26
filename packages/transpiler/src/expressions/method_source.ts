import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {MethodCallChainTranspiler} from "./method_call_chain";

export class MethodSourceTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): string {
    const ret = new MethodCallChainTranspiler().transpile(node, traversal);

    return ret;
  }

}