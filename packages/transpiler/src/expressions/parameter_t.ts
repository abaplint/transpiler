import {Nodes, Expressions} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class ParameterTTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const nameNode = node.findDirectExpression(Expressions.ParameterName);
    const source = traversal.traverse(node.findDirectExpression(Expressions.Target));

    if (nameNode === undefined) {
      throw new Error("ParameterTTranspilerNameNotFound");
    }

    // same key as the EXPORTING side, ParameterNameTranspiler: lower case and namespace
    // escaped, eg. /1BCDWB/FORMOUTPUT becomes $1bcdwb$formoutput
    return new Chunk().appendChunk(traversal.traverse(nameNode)).appendString(": ").appendChunk(source);
  }

}