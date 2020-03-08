import {Nodes, Expressions} from "abaplint";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {SourceTranspiler} from "./source";

export class ParameterSTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode): string {
    const name = node.findDirectExpression(Expressions.ParameterName)?.getFirstToken().getStr();
    const source = new SourceTranspiler().transpile(node.findDirectExpression(Expressions.Source)!);

    return name + " = " + source;
  }

}