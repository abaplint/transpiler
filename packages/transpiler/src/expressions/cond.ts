import {Expressions, Nodes} from "abaplint";
import * as abaplint from "abaplint";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {CompareTranspiler} from ".";

export class CondTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, spaghetti: abaplint.SpaghettiScope, filename: string): string {
// todo, this is not correct
    const int = new CompareTranspiler().transpile(node.findFirstExpression(Expressions.Compare)!, spaghetti, filename);
    return int;
  }

}