import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {FunctionParametersTranspiler} from "./function_parameters";

export class ReceiveParametersTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    return new FunctionParametersTranspiler().transpile(node, traversal);
  }

}