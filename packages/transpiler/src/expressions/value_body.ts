import {Expressions, Nodes, BasicTypes} from "@abaplint/core";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {TypeNameOrInfer} from "./type_name_or_infer";
import {ValueBodyLineTranspiler} from "./value_body_line";
import {FieldAssignmentTranspiler} from "./field_assignment";

export class ValueBodyTranspiler {

  public transpile(typ: Nodes.ExpressionNode, body: Nodes.ExpressionNode, traversal: Traversal): Chunk {

    if (!(typ.get() instanceof Expressions.TypeNameOrInfer)) {
      throw new Error("ValueBodyTranspiler, Expected TypeNameOrInfer");
    }

    const ret = new Chunk().appendString(new TypeNameOrInfer().transpile(typ, traversal).getCode());
    const context = new TypeNameOrInfer().findType(typ, traversal);

    const hasLines = body.findDirectExpression(Expressions.ValueBodyLine) !== undefined;
    let extraFields = "";

    for (const child of body.getChildren()) {
      if (child.get() instanceof Expressions.FieldAssignment && child instanceof Nodes.ExpressionNode) {
        const transpiled = new FieldAssignmentTranspiler().transpile(child, traversal).getCode();
        if (hasLines === false) {
          ret.appendString(transpiled);
        } else {
          extraFields += transpiled;
        }
      } else if (child.get() instanceof Expressions.ValueBodyLine && child instanceof Nodes.ExpressionNode) {
        if (!(context instanceof BasicTypes.TableType)) {
          throw new Error("ValueBodyTranspiler, Expected BasicTypes");
        }
        const rowType = context.getRowType();
        ret.appendString(new ValueBodyLineTranspiler().transpile(rowType, child, traversal, extraFields).getCode());
      } else {
        throw new Error("ValueBodyTranspiler, unknown " + child.get().constructor.name + " " + child.concatTokens());
      }
    }

    return ret;
  }

}