import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";

export class ArithOperatorTranspiler implements IExpressionTranspiler {

  public transpile(_node: Nodes.ExpressionNode): string {
    return ".add(";
  }

}