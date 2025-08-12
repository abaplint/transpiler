import {Expressions, Nodes} from "@abaplint/core";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {TypeNameOrInfer} from "./type_name_or_infer";

export class ValueBodyTranspiler {

  public transpile(typ: Nodes.ExpressionNode, body: Nodes.ExpressionNode, traversal: Traversal): Chunk {

    if (!(typ.get() instanceof Expressions.TypeNameOrInfer)) {
      throw new Error("ValueBodyTranspiler, Expected TypeNameOrInfer");
    }

    const ret = new Chunk().appendString(new TypeNameOrInfer().transpile(typ, traversal).getCode());
    /*
    ret.appendString(".set(");
    // todo: handle LET
    ret.appendString(traversal.traverse(body).getCode());
    ret.appendString(")");
    */
    for (const child of body.getChildren()) {
      if (child.get() instanceof Expressions.FieldAssignment && child instanceof Nodes.ExpressionNode) {
        const field = child.findDirectExpression(Expressions.FieldSub);
        if (field === undefined) {
          throw new Error("ValueBodyTranspiler, Expected FieldSub");
        }
        const source = child.findDirectExpression(Expressions.Source);
        if (source === undefined) {
          throw new Error("ValueBodyTranspiler, Expected Source");
        }
        ret.appendString(".setField(");
        ret.appendString(`"${field.concatTokens().toLowerCase()}"`);
        ret.appendString(", ");
        ret.appendChunk(traversal.traverse(source));
        ret.appendString(")");
      } else {
        throw new Error("ValueBodyTranspiler, unknown " + child.get().constructor.name);
      }
    }

    return ret;
  }

}