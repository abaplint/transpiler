import {Nodes, Expressions} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class ParameterTTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const nameToken = node.findDirectExpression(Expressions.ParameterName)?.getFirstToken();
    const name = nameToken?.getStr();
    const source = traversal.traverse(node.findDirectExpression(Expressions.Target));

    return new Chunk().append(name + ": ", nameToken || node, traversal).appendChunk(source);
  }

}