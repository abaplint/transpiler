import {AbstractType, Expressions, Nodes} from "@abaplint/core";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import * as abaplint from "@abaplint/core";
import {SourceTranspiler} from "./source";

export class FieldAssignmentTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal, context?: AbstractType): Chunk {
    const ret = new Chunk();

    const field = node.findDirectExpression(Expressions.FieldSub);
    if (field === undefined) {
      throw new Error("FieldAssignmentTranspiler, Expected FieldSub");
    }
    const source = node.findDirectExpression(Expressions.Source);
    if (source === undefined) {
      throw new Error("FieldAssignmentTranspiler, Expected Source");
    }

    if (context instanceof abaplint.BasicTypes.StructureType) {
      context = context.getComponentByName(field.concatTokens());
    }

    const sourc = new SourceTranspiler().transpile(source, traversal, context).getCode();
    ret.appendString(`.setField("${field.concatTokens().toLowerCase()}", ${sourc})`);

    return ret;
  }

}