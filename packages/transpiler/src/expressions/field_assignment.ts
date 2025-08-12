import {Expressions, Nodes} from "@abaplint/core";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class FieldAssignmentTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    const ret = new Chunk();

    const field = node.findDirectExpression(Expressions.FieldSub);
    if (field === undefined) {
      throw new Error("FieldAssignmentTranspiler, Expected FieldSub");
    }
    const source = node.findDirectExpression(Expressions.Source);
    if (source === undefined) {
      throw new Error("FieldAssignmentTranspiler, Expected Source");
    }
    ret.appendString(`.setField("${field.concatTokens().toLowerCase()}", ${traversal.traverse(source).getCode()})`);

    return ret;
  }

}