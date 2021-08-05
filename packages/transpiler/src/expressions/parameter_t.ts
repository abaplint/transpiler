import {Nodes, Expressions} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class ParameterTTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const name = node.findDirectExpression(Expressions.ParameterName)?.getFirstToken().getStr();
    const source = traversal.traverse(node.findDirectExpression(Expressions.Target)).getCode();

    return new Chunk(name + ": " + source);
  }

}