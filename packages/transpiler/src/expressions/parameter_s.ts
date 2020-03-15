import {Nodes, Expressions} from "abaplint";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {SourceTranspiler} from "./source";
import {Traversal} from "../traversal";

export class ParameterSTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): string {
    const name = node.findDirectExpression(Expressions.ParameterName)?.getFirstToken().getStr();
    const source = new SourceTranspiler().transpile(node.findDirectExpression(Expressions.Source)!, traversal);

    return name + ": " + source;
  }

}