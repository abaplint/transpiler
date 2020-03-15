import {Nodes, Expressions} from "abaplint";
import * as abaplint from "abaplint";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {SourceTranspiler} from "./source";

export class ParameterSTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, spaghetti: abaplint.SpaghettiScope, filename: string): string {
    const name = node.findDirectExpression(Expressions.ParameterName)?.getFirstToken().getStr();
    const source = new SourceTranspiler().transpile(node.findDirectExpression(Expressions.Source)!, spaghetti, filename);

    return name + ": " + source;
  }

}