import {Nodes} from "abaplint";
import {IExpressionTranspiler} from "./_expression_transpiler";

export class CompareOperatorTranspiler implements IExpressionTranspiler {

  public transpile(_node: Nodes.ExpressionNode): string {
// todo, this is not correct
    return "equals";
  }

}