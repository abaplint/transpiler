import {Expressions, Nodes} from "abaplint";
import * as abaplint from "abaplint";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {ComponentCompareTranspiler} from ".";

export class ComponentCondTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, spaghetti: abaplint.SpaghettiScope, filename: string): string {
// todo, this is not correct
    const int = new ComponentCompareTranspiler().transpile(node.findFirstExpression(Expressions.ComponentCompare)!, spaghetti, filename);
    return int;
  }

}