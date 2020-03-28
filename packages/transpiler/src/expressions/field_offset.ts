import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";

export class FieldOffsetTranspiler implements IExpressionTranspiler {

  public transpile(_node: Nodes.ExpressionNode): string {
// todo
    return "1";
  }

}