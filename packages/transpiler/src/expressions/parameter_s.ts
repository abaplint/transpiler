import {Nodes, Expressions} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class ParameterSTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const name = node.findDirectExpression(Expressions.ParameterName)?.getFirstToken().getStr();
    const source = traversal.traverse(node.findDirectExpression(Expressions.Source)).getCode();

    return new Chunk(name + ": " + source);
  }

}