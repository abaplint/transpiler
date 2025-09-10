import {Nodes, AbstractType, Expressions} from "@abaplint/core";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {TranspileTypes} from "../transpile_types";
import {FieldAssignmentTranspiler} from "./field_assignment";
import {SourceTranspiler} from "./source";

export class ValueBodyLineTranspiler {

  public transpile(rowType: AbstractType, line: Nodes.ExpressionNode, traversal: Traversal, extraFields: string): Chunk {
    const ret = new Chunk();

    ret.appendString(`.appendThis(${TranspileTypes.toType(rowType)}`);

    for (const child of line.getChildren()) {
      if (child instanceof Nodes.TokenNode) {
        // last or first parenthesis
        continue;
      } else if (child.get() instanceof Expressions.FieldAssignment && child instanceof Nodes.ExpressionNode) {
        ret.appendString(new FieldAssignmentTranspiler().transpile(child, traversal).getCode());
      } else if (child.get() instanceof Expressions.Source && child instanceof Nodes.ExpressionNode) {
        // then its a non structured/ table line kind of thing
        ret.appendString(".set(" + new SourceTranspiler().transpile(child, traversal).getCode() + ")");
      } else {
        throw new Error("ValueBodyLineTranspiler, unknown " + child.get().constructor.name + " " + line.concatTokens());
      }
    }

    ret.appendString(extraFields + `)`);

    return ret;
  }

}