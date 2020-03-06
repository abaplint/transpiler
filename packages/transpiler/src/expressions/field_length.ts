import {Nodes} from "abaplint";
import {IExpressionTranspiler} from "./_expression_transpiler";

export class FieldLengthTranspiler implements IExpressionTranspiler {

  public transpile(_node: Nodes.ExpressionNode): string {
// todo
    return "1";
  }

}